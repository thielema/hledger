{-|
Rendering a journal's items back to journal format, reproducing the
original journal file(s). Used by print --export.
-}

module Hledger.Write.Journal (
  journalItemsAsText
) where

import Data.List (intersperse, mapAccumL)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB

import Text.Megaparsec.Pos (SourcePos(..), unPos)

import Hledger.Data.Types

-- | Render journal items as journal text, reproducing the journal file(s) they were parsed from,
-- with transactions rendered from the given (possibly filtered and processed) transactions.
--
-- Directive, comment line, comment block and blank line items are reproduced verbatim.
-- Non-exported directives and include lines are dropped (an included file's items follow inline).
-- Each transaction placeholder is replaced by the rendering of the next given transaction
-- with that source position, or by nothing if there is none (eg it was filtered out).
-- The renderer should not add a trailing blank line, since blank lines are separate items.
-- Any transactions not matching a placeholder (eg from non-journal files) are added at the end,
-- in source position order, separated by blank lines.
journalItemsAsText :: (Transaction -> Text) -> [JournalItem] -> [Transaction] -> TL.Text
journalItemsAsText showtxn items txns = TB.toLazyText $ mconcat $ rendered <> leftovers
  where
    -- transactions queued by source position, in the given order
    txnsbypos = M.map reverse $ M.fromListWith (++) [(posKey $ fst $ tsourcepos t, [t]) | t <- txns]
    (remaining, rendered) = mapAccumL renderItem txnsbypos items
    renderItem m item = case item of
      JITransaction pos -> case M.lookup k m of
        Just (t:ts) -> (M.insert k ts m, TB.fromText $ showtxn t)
        _           -> (m, mempty)
        where k = posKey pos
      JIComment t              -> (m, TB.fromText t)
      JICommentBlock t         -> (m, TB.fromText t)
      JIDirective t            -> (m, TB.fromText t)
      JIBlank t                -> (m, TB.fromText t)
      JINonExportedDirective _ -> (m, mempty)
      JIInclude _              -> (m, mempty)
    -- A map key comparing line and column before the file path, since comparing
    -- SourcePos directly compares the (usually identical) file paths first, which is slow.
    posKey (SourcePos f l c) = (unPos l, unPos c, f)
    leftovers =
      [blankline | not (null items) && not (null leftovertxns)]
      <> intersperse blankline (map (TB.fromText . showtxn) leftovertxns)
      where
        leftovertxns = concat $ M.elems remaining
        blankline = TB.singleton '\n'
