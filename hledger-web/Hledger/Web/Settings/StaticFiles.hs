{-# LANGUAGE TemplateHaskell #-}
{-|
Serving of the web app's static files, and type-safe routes for them.
-}
module Hledger.Web.Settings.StaticFiles where

import Data.FileEmbed (embedDir)
import Network.Wai.Application.Static (defaultFileServerSettings, embeddedSettings, staticApp)
import System.IO (stdout, hFlush)
import WaiAppStatic.Types (MaxAge(..), StaticSettings(..))
import Yesod.Core (WaiSubsite(..))

import Hledger.Web.Settings (staticDir, development, staticFileRoute)

-- | Create the static file serving site (a WAI app used as a Yesod subsite).
-- In development builds it serves the files in the static directory,
-- re-reading them from disk on each request;
-- otherwise it serves the static directory's files as they were
-- at compile time, embedded in the executable.
staticSite :: IO WaiSubsite
staticSite =
  if development
   then do
     putStrLn ("Running in dev mode, will read static files from " ++ staticDir ++ "/") >> hFlush stdout
     return $ serve $ defaultFileServerSettings staticDir
   else
     -- putStrLn "Using built-in web files" >> hFlush stdout
     return $ serve $ embeddedSettings $(embedDir staticDir)
  where
    -- Serve files with hash-based ETag headers, and Cache-Control: no-cache,
    -- so browsers keep them but check with us before each use (a cheap 304 if unchanged).
    -- Otherwise a browser may keep using an old hledger.js after an upgrade or rebuild.
    serve settings = WaiSubsite $ staticApp settings{ssUseHash = True, ssMaxAge = NoCache}

-- Type-safe routes for the static files used by the app,
-- verified to exist at compile time.
css_bootstrap_min_css           = $(staticFileRoute "css/bootstrap.min.css")
hledger_css                     = $(staticFileRoute "hledger.css")
hledger_js                      = $(staticFileRoute "hledger.js")
