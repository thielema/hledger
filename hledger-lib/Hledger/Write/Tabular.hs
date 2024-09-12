{- |
High-level data type for a Spreadsheet table.
A table consists of a main body of data and column and row headers.
The table can be further divided into blocks,
which is encoded in the structure of the headers.

This structure is inspired by the @tabular@ package
but our 'NumLines' data type is an instance of 'Ord'
and our 'Table' type allows for a description of
whether a border belongs to the cell above or below.
-}
module Hledger.Write.Tabular (
    Lines(..),
    NumLines(..),
    Header(..),
    Group(..),
    Side(..),
    headerGroup,
    consGroup,
    consHeaderGroup,
    (./), (/.),
    Table(..),
    Rows(..),
    squish,
    mapRowHeaders,
    mapColumnHeaders,
    transpose,
    fromRowGroup,
    row,
    above,
    numLinesFromProperties,
    downgrade,
    ) where

import qualified Text.Tabular as Tab
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Bifunctor (second)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (sconcat)
import Data.Foldable1 (foldMap1)


class Lines border where noLine :: border
instance Lines () where noLine = ()
instance Lines NumLines where noLine = NoLine
instance Lines Tab.Properties where noLine = Tab.NoLine

{- |
The same as 'Tab.Properties', but has 'Eq' and 'Ord' instances.
We need those for storing 'NumLines' in 'Set's.
We also use 'max' to override 'NoLine' with 'SingleLine' and 'DoubleLine'.
-}
data NumLines = NoLine | SingleLine | DoubleLine
    deriving (Eq, Ord, Show, Enum, Bounded)


{- |
Associate a rule with the previous or subsequent row or column.
The visual appearance should be the same,
however in a spreadsheet processor the difference matters.
We usually want the main body data to be free of borders.
Thus there should be a rule below the header ('After')
and a rule above the footer ('Before').
-}
data Side = Before | After
    deriving (Eq, Ord, Show, Enum, Bounded)

{- |
A header can be an atomic 'Header' or a 'Group' of headers.
-}
data Header h = Header h | HeaderGroup (Group h)
    deriving (Show)

{- |
The group can have @Just label@,
which is a cell that spans multiple atomic cells
and requires an additional row or column.
The 'Side' says,
whether each rule is attached to the left/top or right/bottom cell.
We define a separate data type 'Group'
since the top-level in a table shall be a 'Group'
and not an atomic 'Header'.

'Group's must be non-empty, in order to uniquely assign borders.
-}
data Group h = Group NumLines (Header h, [(Side, Header h)])
    deriving (Show)


instance Functor Header where
    fmap f (Header h) = Header $ f h
    fmap f (HeaderGroup group) = HeaderGroup $ fmap f group

instance Functor Group where
    fmap f (Group border (h,hs)) =
        Group border (fmap f h, map (second $ fmap f) hs)


instance Semigroup (Header h) where
    a<>b = headerGroup NoLine (a, [(After,b)])
    sconcat (h:|hs) = headerGroup NoLine (h, map ((,) After) hs)


headerGroup :: NumLines -> (Header h, [(Side, Header h)]) -> Header h
headerGroup numLines  =  HeaderGroup . Group numLines

consGroup :: Header h -> Side -> Group h -> Group h
consGroup h0 side (Group border (h1,hs)) = Group border (h0,(side,h1):hs)

consHeaderGroup :: Header h -> Maybe (Group h) -> Header h
consHeaderGroup h mhs = maybe h (HeaderGroup . consGroup h After) mhs


infixr 5 ./, /.

(./) :: a -> [(b,a)] -> (a, [(b,a)])
(./) = (,)

(/.) :: b -> (a, [(b,a)]) -> [(b,a)]
b /. (a,bas) = (b,a) : bas


{- |
>>> :{
    let barChar c = case c of NoLine -> '.'; SingleLine -> '|'; DoubleLine -> '#'
        cellText left right text = barChar left : text ++ barChar right : ""
    in  NonEmpty.toList $ squish cellText $ headerGroup DoubleLine $
        Header "header"
        ./ After /.
        sconcat (Header "data 1" :| Header "data 2" : Header "data 3" : [])
        ./ Before /.
        sconcat (Header "footer 1" :| Header "footer 2" : [])
        ./ []
:}
[".header#",".data 1.",".data 2.",".data 3.","#footer 1.",".footer 2."]
-}
squish :: (NumLines -> NumLines -> h -> cell) -> Header h -> NonEmpty cell
squish f =
    let go frontBorder backBorder (Header h) =
            NonEmpty.singleton $ f frontBorder backBorder h
        go frontBorder backBorder (HeaderGroup (Group border (h,hs))) =
            let snoc xs x = NonEmpty.prependList xs $ NonEmpty.singleton x
            in  sconcat $
                NonEmpty.zipWith
                    (\(leftBorder,(sideLeft,x)) (rightBorder,sideRight) ->
                        go
                            (case sideLeft of
                                Before -> border; After -> leftBorder)
                            (case sideRight of
                                Before -> rightBorder; After -> border)
                            x)
                    ((frontBorder,(After,h)) :| map ((,) NoLine) hs)
                    (snoc (map (\(x,_) -> (NoLine,x)) hs) (backBorder,Before))
    in  go NoLine NoLine


{-
ToDo:
A Comfort.Boxed.Array with Header structure encoded in the Shape
would also be a nice data structure for tables.
-}
data Table rh ch a = Table (Maybe (Group rh)) (Maybe (Group ch)) [[a]]
    deriving (Show)

instance Functor (Table rh ch) where
    fmap f (Table rh ch cells) = Table rh ch $ map (map f) cells

transpose :: Table rh ch a -> Table ch rh a
transpose (Table rh ch cells) = Table ch rh $ List.transpose cells


mapRowHeaders :: (rh0 -> rh1) -> Table rh0 ch a -> Table rh1 ch a
mapRowHeaders f (Table rh ch body) = Table (fmap f <$> rh) ch body

mapColumnHeaders :: (ch0 -> ch1) -> Table rh ch0 a -> Table rh ch1 a
mapColumnHeaders f (Table rh ch body) = Table rh (fmap f <$> ch) body


{- |
A Block of a non-empty list of rows.
-}
data Rows rh a = Rows (Header rh) (NonEmpty [a])
    deriving (Show)

rowsHeader :: Rows rh a -> Header rh
rowsHeader (Rows rh _cells) = rh

rowsCells :: Rows rh a -> NonEmpty [a]
rowsCells (Rows _rh cells) = cells

instance Functor (Rows rh) where
    fmap f (Rows rh cells) = Rows rh $ fmap (map f) cells

instance Semigroup (Rows rh a) where
    (<>) = above Before NoLine
    sconcat blocks =
        Rows (sconcat $ fmap rowsHeader blocks) (foldMap1 rowsCells blocks)


fromRowGroup ::
    NumLines -> Maybe (Group ch) ->
    (Rows rh a, [(Side, Rows rh a)]) -> Table rh ch a
fromRowGroup border ch (block,blocks) =
    let rh = Group border (rowsHeader block, map (second rowsHeader) blocks) in
    Table (Just rh) ch $
    concatMap (NonEmpty.toList . rowsCells) (block : map snd blocks)

row :: rh -> [a] -> Rows rh a
row rh cells = Rows (Header rh) $ NonEmpty.singleton cells

above :: Side -> NumLines -> Rows rh a -> Rows rh a -> Rows rh a
above side border (Rows rh0 cells0) (Rows rh1 cells1) =
    Rows (headerGroup border (rh0,[(side,rh1)])) (cells0<>cells1)


-- * Conversion to @tabular@ package

numLinesFromProperties :: Tab.Properties -> NumLines
numLinesFromProperties Tab.NoLine = NoLine
numLinesFromProperties Tab.SingleLine = SingleLine
numLinesFromProperties Tab.DoubleLine = DoubleLine

propertiesFromNumLines :: NumLines -> Tab.Properties
propertiesFromNumLines NoLine = Tab.NoLine
propertiesFromNumLines SingleLine = Tab.SingleLine
propertiesFromNumLines DoubleLine = Tab.DoubleLine


downgradeHeader :: Header h -> Tab.Header h
downgradeHeader =
    let go (Header h) = Tab.Header h
        go (HeaderGroup (Group numLines (h,hs))) =
            Tab.Group (propertiesFromNumLines numLines) $
            map go $ h : map snd hs
    in  go

downgradeGroup :: Maybe (Group h) -> Tab.Header h
downgradeGroup Nothing = Tab.Group Tab.NoLine []
downgradeGroup (Just g) = downgradeHeader $ HeaderGroup g

downgrade :: Table rh ch a -> Tab.Table rh ch a
downgrade (Table rh ch cells) =
    Tab.Table (downgradeGroup rh) (downgradeGroup ch) cells
