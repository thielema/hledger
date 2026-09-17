{-|

The @transactions@ command lists transactions one per line, like @print --oneline@.

-}

{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Transactions (
  transactionsmode
 ,transactions
) where

import System.Console.CmdArgs.Explicit

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Print (print')


-- | Command line options for this command.
transactionsmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Transactions.txt")
  [
   let arg = "DESC" in
   flagReq  ["match","m"] (\s opts -> Right $ setopt "match" s opts) arg
    ("fuzzy search for one recent transaction with description closest to "++arg)
  ]
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The transactions command: print's one-line view of each transaction.
transactions :: CliOpts -> Journal -> IO ()
transactions opts = print' opts{rawopts_ = setboolopt "oneline" $ rawopts_ opts}
