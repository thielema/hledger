{-|
List of all texts the CLI program can output in different languages
and related utilities.
-}

module Hledger.Cli.Message (
   Message(..)
  ,getText
)
where

import Data.Default (Default(def))
import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
-- import Text.Printf (printf)

import Data.Gettext (Catalog, gettext)


-- | List of all hledger text messages
data Message =
    None
  | Account
  | Revenues
  | Expenses
  | Assets
  | Liabilities
  | Equity
  | CashFlows
  | Net
  | Total
  | Average
  | RightTotal
  | RightAverage
  | Commodity
  | Balance
  | Budget
  | BalanceSheet
  | BalanceSheetWithEquity
  | IncomeStatement
  | CashflowStatement
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Default Message where
  def = None

-- ToDo: We must ensure that a Catalog is always available, we must embed a standard catalog
getText :: Maybe Catalog -> Message -> Text
getText Nothing = T.pack . show
getText (Just cat) = TL.toStrict . gettext cat . B.pack . show
