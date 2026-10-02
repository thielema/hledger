{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Web.Widget.ReportConfig where

import Control.Applicative ((<|>))
import Control.Monad (when, guard)
import Control.Arrow ((&&&))
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Text.Blaze ((!), textValue)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Read (readMaybe)
import Yesod

import Hledger


-- | Links to the balance report page, single-period and for each
-- interval, carrying the current search and date span; the report being
-- shown, identified by the interval it was built with, is marked.
balanceReportForm :: r -> [(Text, Text)] -> Text -> DateSpan -> Interval -> HtmlUrl r
balanceReportForm balanceR params qparam spn current render = do
  H.div ! A.class_ "report-form-container" $ do
    H.h1 "Generate Report"
    H.form ! A.method "get" ! A.action (textValue $ render balanceR params) $ do
      H.table $ do
        H.tr $ do
          H.td "Report type:"
          H.td $ selectButton "report" reportTypes Nothing params

        H.tr $ do
          H.td "Period:"
          H.td $ selectButtonIntern "period" intervalOptions current

        H.tr $ do
          H.td "Layout:"
          H.td $ selectButton "layout" layoutOptions Nothing params

        H.tr $ do
          H.td "Account Structure:"
          H.td $ selectButton "structure" structureOptions Nothing params

        H.tr $ do
          H.td "Depth:"
          let name = "depth"
          H.td $ selectButtonIntern name depthOptions $
            parseDepth =<< lookup name params

        H.tr $ do
          H.td "Empty accounts:"
          H.td $ selectButton "empty" emptinessOptions Nothing params

      when (not (T.null qparam)) $
        H.input
          ! A.type_ "hidden"
          ! A.name "q"
          ! A.value (H.toValue qparam)

      H.div ! A.class_ "form-group" $ do
        H.button
          ! A.type_ "submit"
          ! A.name "action"
          ! A.value "generate" $ "Generate"


selectButton :: Text -> [Option a] -> Maybe Text -> [(Text, Text)] -> Html
selectButton name available deflt params =
  let current = lookup name params <|> deflt in
  selectButtonGen name available
    (\(Option _label _intVal val) -> Just val == current)

selectButtonIntern :: (Eq a) => Text -> [Option a] -> a -> Html
selectButtonIntern name available current =
  selectButtonGen name available
    (\(Option _label intVal _val) -> intVal == current)

selectButtonGen :: Text -> [Option a] -> (Option a -> Bool) -> Html
selectButtonGen name available current =
  let nameVal = H.toValue name in
  H.select ! A.id nameVal ! A.name nameVal $ do
    for_ available $ \opt@(Option label _haskellValue val) ->
      let option = H.option ! A.value (H.toValue val) $ toHtml label in
      if current opt
        then option ! A.selected "true"
        else option


data Report =
    Balance
  | Budget
  deriving (Eq, Ord, Enum, Bounded, Show)

{-
We cannot use OptionList data type and benefit from mkOptionList,
because OptionList has an additional constructor OptionListGrouped
that we have to case test on.
-}
reportTypes :: [Option Report]
reportTypes =
  [ Option "Balance" Balance "balance"
  , Option "Budget" Budget "budget"
  ]

intervalOptions :: [Option Interval]
intervalOptions =
  [ Option "Single period" (NoInterval) ""
  , Option "Yearly"        (Years 1)    "yearly"
  , Option "Quarterly"     (Quarters 1) "quarterly"
  , Option "Monthly"       (Months 1)   "monthly"
  , Option "Weekly"        (Weeks 1)    "weekly"
  , Option "Daily"         (Days 1)     "daily"
  ]

layoutOptions :: [Option Layout]
layoutOptions =
  [ Option "Wide"      (LayoutWide Nothing) "wide"
  , Option "Tall"      (LayoutTall)         "tall"
  , Option "Bare"      (LayoutBare)         "bare"
  , Option "Bare-Wide" (LayoutBareWide)     "barewide"
  ]

structureOptions :: [Option (AccountListMode, Bool, Bool)]
structureOptions =
  [ Option "List mode" (ALFlat, False, False) "flat"
  , Option "Tree mode with parent account elision" (ALTree, False, False) "tree-elide"
  , Option "Tree mode with all parent accounts" (ALTree, False, True) "tree"
  , Option "Tree mode with full names and elision" (ALTree, True, False) "tree-full-names-elide"
  , Option "Tree mode with full names and parent accounts" (ALTree, True, True) "tree-full-names"
  ]

depthOptions :: [Option (Maybe Int)]
depthOptions =
  flip map [1..(10::Int)]
      (\n -> let str = T.pack $ show n in Option str (Just n) str)
  ++
  [ Option "Unlimited" Nothing "" ]

parseDepth :: Text -> Maybe Int
parseDepth str = do
  guard $ T.all isDigit str
  guard $ T.null $ T.drop 3 str
  readMaybe $ T.unpack str

emptinessOptions :: [Option (Maybe Bool)]
emptinessOptions =
  [ Option "Default" Nothing ""
  , Option "Show" (Just False) "false"
  , Option "Hide" (Just True) "true"
  ]

parseExternal :: [Option a] -> Text -> Maybe a
parseExternal =
  flip Map.lookup . Map.fromList .
  map (optionExternalValue &&& optionInternalValue)
