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
    Table(..),
    Rows(..),
    squish,
    mapRowHeaders,
    mapColumnHeaders,
    transpose,
    fromRowGroup,
    consFromRowGroup,
    row,
    above,
    stack,
    numLinesFromProperties,
    downgrade,
    ) where

import qualified Text.Tabular as Tab
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List


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

{-
ToDo:
Should the Header list be NonEmpty?
Otherwise border lines are lost when there is no header or no footer row.
-}
{- |
The group can have @Just label@,
which is a cell that spans multiple atomic cells
and requires an additional row or column.
The 'Side' says,
whether each rule is attached to the left/top or right/bottom cell.
We define a separate data type 'Group'
since the top-level in a table shall be a 'Group'
and not an atomic 'Header'.
-}
data Group h = Group Side NumLines [Header h]
    deriving (Show)


instance Functor Header where
    fmap f (Header h) = Header $ f h
    fmap f (HeaderGroup group) = HeaderGroup $ fmap f group

instance Functor Group where
    fmap f (Group side border hs) = Group side border $ map (fmap f) hs


headerGroup :: Side -> NumLines -> [Header h] -> Header h
headerGroup side numLines  =  HeaderGroup . Group side numLines

consGroup :: Header h -> Group h -> Group h
consGroup h (Group side border hs) = Group side border $ h:hs


{- |
>>> :{
    let barChar c = case c of NoLine -> '.'; SingleLine -> '|'; DoubleLine -> '#'
        cellText left right text = barChar left : text ++ barChar right : ""
    in  squish cellText $
        headerGroup After DoubleLine [Header "header", headerGroup Before SingleLine [headerGroup After NoLine [Header "data 1", Header "data 2", Header "data 3"], headerGroup After NoLine [Header "footer 1", Header "footer 2"]]]
:}
[".header#",".data 1.",".data 2.",".data 3.","|footer 1.",".footer 2."]
-}
squish :: (NumLines -> NumLines -> h -> cell) -> Header h -> [cell]
squish f =
    let go frontBorder backBorder (Header h) = [f frontBorder backBorder h]
        go frontBorder backBorder (HeaderGroup (Group side border hs)) =
            let snoc xs x = NonEmpty.prependList xs $ NonEmpty.singleton x
                (border0,border1) =
                    case side of
                        Before -> (border, NoLine)
                        After -> (NoLine, border)
                frontBorders = frontBorder : repeat border0
                backBorders = NonEmpty.tail $ snoc (border1<$hs) backBorder
            in  concat $ zipWith3 go frontBorders backBorders hs
    in  go NoLine NoLine


{-
ToDo:
A Comfort.Boxed.Array with Header structure encoded in the Shape
would also be a nice data structure for tables.
-}
data Table rh ch a = Table (Group rh) (Group ch) [[a]]
    deriving (Show)

instance Functor (Table rh ch) where
    fmap f (Table rh ch cells) = Table rh ch $ map (map f) cells

transpose :: Table rh ch a -> Table ch rh a
transpose (Table rh ch cells) = Table ch rh $ List.transpose cells


mapRowHeaders :: (rh0 -> rh1) -> Table rh0 ch a -> Table rh1 ch a
mapRowHeaders f (Table rh ch body) = Table (fmap f rh) ch body

mapColumnHeaders :: (ch0 -> ch1) -> Table rh ch0 a -> Table rh ch1 a
mapColumnHeaders f (Table rh ch body) = Table rh (fmap f ch) body


data Rows rh a = Rows (Header rh) [[a]]
    deriving (Show)

rowsHeader :: Rows rh a -> Header rh
rowsHeader (Rows rh _cells) = rh

rowsCells :: Rows rh a -> [[a]]
rowsCells (Rows _rh cells) = cells

instance Functor (Rows rh) where
    fmap f (Rows rh cells) = Rows rh $ map (map f) cells

instance Semigroup (Rows rh a) where
    (<>) = above Before NoLine

instance Monoid (Rows rh a) where
    mappend = (<>)
    mconcat = stack Before NoLine


fromRowGroup ::
    Side -> NumLines -> Group ch -> [Rows rh a] -> Table rh ch a
fromRowGroup side border ch blocks =
    Table (Group side border $ map rowsHeader blocks) ch $
    concatMap rowsCells blocks

consFromRowGroup ::
    Side -> NumLines -> Table rh ch a -> [Rows rh a] -> Table rh ch a
consFromRowGroup side border (Table rh ch cells) blocks =
    Table (Group side border $ HeaderGroup rh : map rowsHeader blocks) ch $
    cells ++ concatMap rowsCells blocks

row :: rh -> [a] -> Rows rh a
row rh cells = Rows (Header rh) [cells]

above :: Side -> NumLines -> Rows rh a -> Rows rh a -> Rows rh a
above side border (Rows rh0 cells0) (Rows rh1 cells1) =
    Rows (headerGroup side border [rh0,rh1]) (cells0++cells1)

stack :: Side -> NumLines -> [Rows rh a] -> Rows rh a
stack side border blocks =
    Rows (headerGroup side border $ map rowsHeader blocks) $
    concatMap rowsCells blocks


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
        go (HeaderGroup (Group _ numLines hs)) =
            Tab.Group (propertiesFromNumLines numLines) $ map go hs
    in  go

downgrade :: Table rh ch a -> Tab.Table rh ch a
downgrade (Table rh ch cells) =
    Tab.Table
        (downgradeHeader $ HeaderGroup rh)
        (downgradeHeader $ HeaderGroup ch)
        cells
