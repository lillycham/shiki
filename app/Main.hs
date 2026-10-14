{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative (Parser, ParserInfo, argument, command, execParser, fullDesc, header, helper, hsubparser, info, metavar, progDesc, str, (<**>))
import Shiki.Server (checkOnce, serve)
import Shiki.Types (Config (cfgHost, cfgPort), Result (resDetail, resGroup, resName, resState, resSummary), State (Fail), loadConfig, stateText)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

data Cmd = Serve FilePath | Check FilePath

cmdParser :: Parser Cmd
cmdParser =
  hsubparser
    ( command "serve" (info (Serve <$> configArg) (progDesc "Run the probes on their timers, and serve the results as JSON."))
        <> command "check" (info (Check <$> configArg) (progDesc "Run every probe once and print the results. Exits 1 if one fails."))
    )
  where
    configArg = argument str (metavar "CONFIG.json")

opts :: ParserInfo Cmd
opts = info (cmdParser <**> helper) (fullDesc <> header "shiki - small probes that report back")

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Serve path -> withConfig path $ \cfg -> do
      hPutStrLn stderr ("shiki: serving on http://" ++ cfgHost cfg ++ ":" ++ show (cfgPort cfg) ++ "/probes")
      serve cfg
    Check path -> withConfig path $ \cfg -> do
      rs <- checkOnce cfg
      mapM_ (TIO.putStrLn . line) rs
      if any ((== Fail) . resState) rs then exitWith (ExitFailure 1) else pure ()
  where
    withConfig path k = loadConfig path >>= either (\e -> hPutStrLn stderr ("shiki: " ++ path ++ ": " ++ e) >> exitWith (ExitFailure 2)) k
    line r =
      T.intercalate
        "  "
        [ T.justifyLeft 7 ' ' (stateText (resState r)),
          T.justifyLeft 24 ' ' (resGroup r <> "/" <> resName r),
          resSummary r <> maybe "" (\d -> "  (" <> d <> ")") (resDetail r)
        ]
