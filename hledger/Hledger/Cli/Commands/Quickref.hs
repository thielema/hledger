{-|

The quick reference card: a compact overview of hledger's most useful
commands, flags, and concepts, shown by @hledger help quickref@.

-}

{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Quickref (
  showQuickref
) where

import Data.List (isPrefixOf, isSuffixOf)
import System.Console.ANSI (setSGRCode, SGR(..), Underlining(..))

import Hledger
import Hledger.Cli.CliOptions (prognameandversionnoarch)

-- | The quick reference card, from quickref.txt in the hledger package directory.
quickrefcontent :: String
quickrefcontent = $(embedFileRelative "quickref.txt")

-- | Print the quick reference card, styling it a little when color is enabled.
showQuickref :: IO ()
showQuickref = do
  usecolor <- useColorOnStdout
  runPager $ styleQuickref usecolor quickrefcontent

-- | Render the card. The program name and version are added right-aligned on
-- the title line. When color is enabled: the title line gets hledger's usual
-- bold colour gradient (and the version a faint gradient), labels in the margin
-- (the first 18 columns) are faint, top-level headings are also underlined (up to
-- their first : or ,), a heading followed by just a parenthesised note is all faint,
-- and the commands, flags etc. beyond the margin are left normal, except parentheses,
-- which are faint.
styleQuickref :: Bool -> String -> String
styleQuickref usecolor s = unlines $ case lines s of
  []         -> []
  title:rest -> titleline title : map styleline rest
  where
    version = prognameandversionnoarch
    -- align the version's right edge with the widest line of the card
    cardwidth = maximum $ map length $ lines s
    titleline t = titleAndVersionLine cardwidth t version
    labelwidth = 18
    styleheading = faint'
    styleline l
      | not usecolor      = l
      | all (==' ') label = label <> dimparens content
      | isnoteheading     = stylelabel l
      | otherwise         = stylelabel label <> dimparens content
      where
        -- eg "Common flags: (hledger -h ... to see all)"
        isnoteheading = case break (==':') l of
          (_:_, _:rest) -> let note = dropWhile (==' ') rest in "(" `isPrefixOf` note && ")" `isSuffixOf` note
          _             -> False
        (label, content) = splitAt labelwidth l
    -- an indented sub-label is just faint; a top-level heading's name is also underlined
    stylelabel t@(' ':_) = styleheading t
    stylelabel t         = underline' (styleheading name) <> styleheading rest
      where (name, rest) = break (`elem` (",:" :: String)) t
    dimparens = concatMap (\c -> if c `elem` "()" then styleheading [c] else [c])

-- | Wrap a string in ANSI codes to switch single underlining on and off.
underline' :: String -> String
underline' s = setSGRCode [SetUnderlining SingleUnderline] <> s <> setSGRCode [SetUnderlining NoUnderline]
