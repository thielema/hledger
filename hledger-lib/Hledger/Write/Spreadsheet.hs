{- |
Describe a Spreadsheet table as array (list of lists) of cells.
Cells can have some style attributes.
This is the basis for ODS and HTML export.
-}
module Hledger.Write.Spreadsheet (
    Type(..),
    Style(..),
    Emphasis(..),
    Cell(..),
    Class(Class), textFromClass,
    Border(..),
    Lines(..),
    NumLines(..),
    noBorder,
    defaultCell,
    emptyCell,
    transposeCell,
    transpose,
    Content,
    setBorders,
    flattenTable,
    flattenTable0,
    flattenRows0,
    ) where

import qualified Hledger.Write.Tabular as Tab
import Hledger.Write.Tabular (Lines(..), NumLines(..))
import Hledger.Data.Types (Amount)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)


data Type =
      TypeString
    | TypeAmount !Amount
    | TypeMixedAmount
    | TypeDate
    deriving (Eq, Ord, Show)

data Style = Body Emphasis | Head
    deriving (Eq, Ord, Show)

data Emphasis = Item | Total
    deriving (Eq, Ord, Show)


data Border lines =
    Border {
        borderLeft, borderRight,
        borderTop, borderBottom :: lines
    }
    deriving (Eq, Ord, Show)

instance Functor Border where
    fmap f (Border left right top bottom) =
        Border (f left) (f right) (f top) (f bottom)

instance Applicative Border where
    pure a = Border a a a a
    Border fLeft fRight fTop fBottom <*> Border left right top bottom =
        Border (fLeft left) (fRight right) (fTop top) (fBottom bottom)

instance Foldable Border where
    foldMap f (Border left right top bottom) =
        f left <> f right <> f top <> f bottom

noBorder :: (Lines border) => Border border
noBorder = pure noLine

instance (Ord lines) => Semigroup (Border lines) where
    (<>) = liftA2 max

instance (Lines lines, Ord lines) => Monoid (Border lines) where
    mempty = noBorder
    mappend = (<>)

transposeBorder :: Border lines -> Border lines
transposeBorder (Border left right top bottom) =
    Border top bottom left right


newtype Class = Class Text

textFromClass :: Class -> Text
textFromClass (Class cls) = cls

data Cell border text =
    Cell {
        cellType :: Type,
        cellBorder :: Border border,
        cellStyle :: Style,
        cellAnchor :: Text,
        cellClass :: Class,
        cellContent :: text
    }

instance Functor (Cell border) where
    fmap f (Cell typ border style anchor class_ content) =
        Cell typ border style anchor class_ $ f content

defaultCell :: (Lines border) => text -> Cell border text
defaultCell text =
    Cell {
        cellType = TypeString,
        cellBorder = noBorder,
        cellStyle = Body Item,
        cellAnchor = mempty,
        cellClass = Class mempty,
        cellContent = text
    }

emptyCell :: (Lines border, Monoid text) => Cell border text
emptyCell = defaultCell mempty

mapCellBorder ::
    (Border border0 -> Border border1) -> Cell border0 text -> Cell border1 text
mapCellBorder f cell = cell {cellBorder = f $ cellBorder cell}

transposeCell :: Cell border text -> Cell border text
transposeCell = mapCellBorder transposeBorder

transpose :: [[Cell border text]] -> [[Cell border text]]
transpose = List.transpose . map (map transposeCell)



setBorders :: Cell () text -> Border lines -> Cell lines text
setBorders cell border = cell {cellBorder = border}

adaptBorders :: Cell NumLines text -> Border NumLines -> Cell NumLines text
adaptBorders cell border =
    cell {cellBorder = border <> cellBorder cell}

combineBorders ::
    Cell () text -> Border NumLines -> Border NumLines -> Cell NumLines text
combineBorders cell rh ch =
    adaptBorders (setBorders cell noBorder) $
    Border
        (borderLeft   ch)
        (borderRight  ch)
        (borderTop    rh)
        (borderBottom rh)


squishNonEmpty ::
    (NumLines -> NumLines -> h -> a) ->
    Tab.Header h -> Maybe (Tab.Group h) -> NonEmpty a
squishNonEmpty f h hs = Tab.squish f $ Tab.consHeaderGroup h hs

squishMaybeEmpty ::
    (NumLines -> NumLines -> h -> a) -> Maybe (Tab.Group h) -> [a]
squishMaybeEmpty f =
    maybe [] (NonEmpty.toList . Tab.squish f . Tab.HeaderGroup)

flattenTable1 ::
    Cell () text ->
    Tab.Table (Cell () text) (Cell () text) (Cell () text) ->
    [[Cell NumLines text]]
flattenTable1 leftTop (Tab.Table rhs chs cells) =
    let (rfh:|rfhs) =
            squishNonEmpty
                (\topBorder bottomBorder cell ->
                    setBorders cell $
                    Border NoLine NoLine topBorder bottomBorder)
                (Tab.Header leftTop) rhs in
    let cfhs = map cellBorder $ NonEmpty.tail headerRow
        headerRow =
            squishNonEmpty
                (\leftBorder rightBorder cell ->
                    adaptBorders cell $
                    cellBorder rfh <>
                    Border leftBorder rightBorder NoLine NoLine)
                (Tab.Header rfh) (fmap (flip setBorders noBorder <$>) chs) in

    NonEmpty.toList headerRow
    :
    zipWith
        (\rh row ->
            rh :
            zipWith
                (\ch cell -> combineBorders cell ch (cellBorder rh))
                cfhs row)
        rfhs
        cells

flattenTable0 ::
    Tab.Table () (Cell () text) (Cell () text) -> [[Cell NumLines text]]
flattenTable0 (Tab.Table rhs chs cells) =
    let (rfh:|rfhs) =
            squishNonEmpty
                (\topBorder bottomBorder () ->
                    Border NoLine NoLine topBorder bottomBorder)
                (Tab.Header ()) rhs in
    let cfhs = map cellBorder headerRow
        headerRow =
            squishMaybeEmpty
                (\leftBorder rightBorder cell ->
                    setBorders cell $
                    rfh <> Border leftBorder rightBorder NoLine NoLine)
                chs in

    headerRow
    :
    zipWith
        (\rh -> zipWith (\ch cell -> combineBorders cell rh ch) cfhs)
        rfhs
        cells

flattenRows0 ::
    Maybe (Tab.Group ch) ->
    Tab.Rows () (Cell () text) -> NonEmpty [Cell NumLines text]
flattenRows0 chs (Tab.Rows rhs cells) =
    let rfhs =
            Tab.squish
                (\topBorder bottomBorder () ->
                    Border NoLine NoLine topBorder bottomBorder)
                rhs in
    let cfhs =
            squishMaybeEmpty
                (\leftBorder rightBorder _ch ->
                    Border leftBorder rightBorder NoLine NoLine)
                chs in

    NonEmpty.zipWith
        (\rh -> zipWith (\ch cell -> combineBorders cell rh ch) cfhs)
        rfhs
        cells

{-
For symmetry reasons
we would need all four combinations of pairs of () and Text,
also because tables can be transposed.
However, it is easier to do transposition of the flat cell array,
because the types of original and transposed table mismatch.
-}
class Content content where
    flattenTable ::
        Tab.Table content Text (Cell () Text) ->
        ((Maybe Int, Maybe Int), [[Cell NumLines Text]])

instance Content Text where
    flattenTable =
        (,) (Just 1, Just 1) . flattenTable1 emptyCell .
        Tab.mapRowHeaders defaultCell .
        Tab.mapColumnHeaders defaultCell

instance Content () where
    flattenTable =
        (,) (Just 1, Nothing) . flattenTable0 . Tab.mapColumnHeaders defaultCell
