{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket)
import Data.Aeson (Value (Null), eitherDecode, object, (.=))
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Shiki.Probe (LaunchdInfo (LaunchdInfo, ldLastExit, ldPid, ldRunning, ldRuns), ago, judgeAge, judgeCount, judgeExit, judgeGit, judgeHttp, judgeJsonArray, judgeLaunchd, parseLaunchd, runProbe)
import Shiki.Server (groupJson)
import Shiki.Types (Config (cfgPort, cfgProbes), Kind (Command, DirCount, FileAge, Launchd), Output (ExitStatus, JsonArray), Probe (Probe, probeKind, probeName), Result (resState, resSummary), State (Fail, Ok, Unknown, Warn), validate, worst)
import System.Directory (createDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "shiki" [stateTests, launchdTests, judgeTests, jsonArrayTests, configTests, groupTests, probeTests]

stateTests :: TestTree
stateTests =
  testGroup
    "states"
    [ testCase "worst picks the worst" $ worst [Ok, Warn, Unknown] @?= Warn,
      testCase "fail beats everything" $ worst [Warn, Fail, Ok] @?= Fail,
      testCase "unknown is worse than ok" $ worst [Ok, Unknown] @?= Unknown,
      testCase "no states is unknown" $ worst [] @?= Unknown
    ]

-- Trimmed output of `launchctl print gui/501/akyuu-backup` on macOS 15.
launchdSample :: Text
launchdSample =
  "gui/501/akyuu-backup = {\n\
  \\tactive count = 0\n\
  \\tpath = /Users/me/Library/LaunchAgents/akyuu-backup.plist\n\
  \\ttype = LaunchAgent\n\
  \\tstate = not running\n\
  \\n\
  \\tprogram = /bin/sh\n\
  \\tevent triggers = {\n\
  \\t\tstate = 1\n\
  \\t}\n\
  \\truns = 3\n\
  \\tlast exit code = 0\n\
  \}\n"

launchdTests :: TestTree
launchdTests =
  testGroup
    "launchd"
    [ testCase "reads the top-level fields only" $
        parseLaunchd launchdSample @?= LaunchdInfo {ldRunning = False, ldPid = Nothing, ldLastExit = Just "0", ldRuns = Just 3},
      testCase "reads a running job" $
        parseLaunchd "x = {\n\tstate = running\n\tpid = 4242\n\tlast exit code = (never exited)\n}\n"
          @?= LaunchdInfo {ldRunning = True, ldPid = Just 4242, ldLastExit = Just "(never exited)", ldRuns = Nothing},
      testCase "an idle job that exited 0 is ok" $
        judgeLaunchd False (parseLaunchd launchdSample) @?= (Ok, "idle · exit 0 · 3 runs", Nothing),
      testCase "a job that must run and does not fails" $
        fst3 (judgeLaunchd True (parseLaunchd launchdSample)) @?= Fail,
      testCase "a bad exit code fails" $
        judgeLaunchd False (info False (Just "78: Function not implemented")) @?= (Fail, "last exit 78: Function not implemented", Nothing),
      testCase "never exited is fine" $
        fst3 (judgeLaunchd True (info True (Just "(never exited)"))) @?= Ok
    ]
  where
    info r e = LaunchdInfo {ldRunning = r, ldPid = Nothing, ldLastExit = e, ldRuns = Nothing}

judgeTests :: TestTree
judgeTests =
  testGroup
    "judges"
    [ testCase "ages read short" $ map ago [5, 300, 3 * 3600 + 720, 3 * 86400] @?= ["just now", "5m ago", "3h 12m ago", "3d ago"],
      testCase "young is ok" $ fst3 (judgeAge 3600 Nothing 60) @?= Ok,
      testCase "old warns" $ fst3 (judgeAge 3600 Nothing 7200) @?= Warn,
      testCase "very old fails" $ fst3 (judgeAge 3600 (Just 7200) 9000) @?= Fail,
      testCase "few entries is ok" $ judgeCount 10 Nothing 1 @?= (Ok, "1 entry", Nothing),
      testCase "many entries warn" $ judgeCount 10 (Just 50) 12 @?= (Warn, "12 entries", Nothing),
      testCase "too many entries fail" $ fst3 (judgeCount 10 (Just 50) 51) @?= Fail,
      testCase "the expected status is ok" $ judgeHttp 200 200 12 @?= (Ok, "200 · 12 ms", Nothing),
      testCase "another status fails" $ fst3 (judgeHttp 200 502 3) @?= Fail,
      testCase "a pushed repository is ok" $ judgeGit Nothing 600 (Just (Right 0)) @?= (Ok, "last commit 10m ago · pushed", Nothing),
      testCase "unpushed commits warn" $ judgeGit Nothing 600 (Just (Right 2)) @?= (Warn, "last commit 10m ago · 2 commits not pushed", Nothing),
      testCase "an old commit warns" $ fst3 (judgeGit (Just 86400) (2 * 86400) Nothing) @?= Warn,
      testCase "exit 0 shows the last line" $ judgeExit ExitSuccess "one\ntwo\n" "" @?= (Ok, "two", Nothing),
      testCase "another exit fails with the error" $ judgeExit (ExitFailure 3) "" "boom\n" @?= (Fail, "exit 3", Just "boom")
    ]

jsonArrayTests :: TestTree
jsonArrayTests =
  testGroup
    "json arrays"
    [ testCase "problems warn, counted by severity" $
        judgeJsonArray M.empty (M.fromList [("severity", "problem")]) (Just "severity") findings
          @?= (Warn, "2 problem · 1 info", Nothing),
      testCase "a fail match fails" $
        fst3 (judgeJsonArray (M.fromList [("severity", "problem")]) M.empty Nothing findings) @?= Fail,
      testCase "no rules is ok" $ judgeJsonArray M.empty M.empty Nothing findings @?= (Ok, "3 items", Nothing),
      testCase "an empty array is ok" $ judgeJsonArray M.empty (M.fromList [("severity", "problem")]) Nothing (eitherJson "[]") @?= (Ok, "none", Nothing),
      testCase "not an array is unknown" $ fst3 (judgeJsonArray M.empty M.empty Nothing Null) @?= Unknown
    ]
  where
    findings = eitherJson "[{\"severity\":\"problem\"},{\"severity\":\"info\"},{\"severity\":\"problem\"}]"
    eitherJson s = either error id (eitherDecode s)

configTests :: TestTree
configTests =
  testGroup
    "config"
    [ testCase "reads every kind, with defaults" $ do
        c <- parse goodConfig
        cfgPort c @?= 8092
        map probeName (cfgProbes c) @?= ["backup", "inbox", "check"]
        map probeKind (cfgProbes c)
          @?= [ Launchd "akyuu-backup" False,
                DirCount "/tmp/inbox" 20 Nothing,
                Command ["akyuu", "check", "--json"] (JsonArray M.empty (M.fromList [("severity", "problem")]) (Just "severity"))
              ],
      testCase "refuses an unknown kind" $ refused "{\"probes\":[{\"name\":\"a\",\"kind\":\"ping\"}]}",
      testCase "refuses an empty command" $ refused "{\"probes\":[{\"name\":\"a\",\"kind\":\"command\",\"argv\":[]}]}",
      testCase "refuses two probes of one name" $
        refused "{\"probes\":[{\"name\":\"a\",\"kind\":\"http\",\"url\":\"http://x\"},{\"name\":\"a\",\"kind\":\"http\",\"url\":\"http://y\"}]}",
      testCase "refuses a zero interval" $ refused "{\"probes\":[{\"name\":\"a\",\"every\":0,\"kind\":\"http\",\"url\":\"http://x\"}]}"
    ]
  where
    goodConfig =
      "{\"probes\":[\
      \{\"name\":\"backup\",\"group\":\"akyuu\",\"kind\":\"launchd\",\"label\":\"akyuu-backup\"},\
      \{\"name\":\"inbox\",\"kind\":\"dir-count\",\"path\":\"/tmp/inbox\",\"warn-above\":20},\
      \{\"name\":\"check\",\"kind\":\"command\",\"argv\":[\"akyuu\",\"check\",\"--json\"],\"output\":\"json-array\",\
      \\"warn-where\":{\"severity\":\"problem\"},\"count-by\":\"severity\"}]}"
    parse s = either assertFailure pure (eitherDecode s >>= validate)
    refused s = case eitherDecode s >>= validate of
      Left _ -> pure ()
      Right c -> assertFailure ("accepted " ++ show (c :: Config))

groupTests :: TestTree
groupTests =
  testCase "a group counts its states, worst first" $
    groupJson "g" [] @?= object ["group" .= ("g" :: Text), "state" .= ("unknown" :: Text), "summary" .= ("" :: Text), "probes" .= ([] :: [Value])]

probeTests :: TestTree
probeTests =
  testGroup
    "running probes"
    [ testCase "a command that exits 0 is ok" $ do
        r <- run (Command ["/bin/sh", "-c", "echo fine"] ExitStatus)
        (resState r, resSummary r) @?= (Ok, "fine"),
      testCase "a command that exits 1 fails" $ do
        r <- run (Command ["/bin/sh", "-c", "exit 1"] ExitStatus)
        resState r @?= Fail,
      testCase "a slow command times out" $ do
        r <- run (Command ["/bin/sleep", "5"] ExitStatus)
        (resState r, resSummary r) @?= (Fail, "timed out after 1 s"),
      testCase "a missing command fails, and does not throw" $ do
        r <- run (Command ["/nonexistent/shiki-test"] ExitStatus)
        resState r @?= Fail,
      testCase "a fresh file is ok" $ withTempDir $ \d -> do
        writeFile (d </> "f") "x"
        r <- run (FileAge (d </> "f") 3600 Nothing)
        (resState r, resSummary r) @?= (Ok, "just now"),
      testCase "an empty directory fails the age probe" $ withTempDir $ \d -> do
        r <- run (FileAge d 3600 Nothing)
        (resState r, resSummary r) @?= (Fail, "empty"),
      testCase "a missing file fails the age probe" $ do
        r <- run (FileAge "/nonexistent/shiki-test" 3600 Nothing)
        (resState r, resSummary r) @?= (Fail, "missing"),
      testCase "dot files do not count" $ withTempDir $ \d -> do
        mapM_ (\n -> writeFile (d </> n) "x") ["a", "b", ".DS_Store"]
        r <- run (DirCount d 1 Nothing)
        (resState r, resSummary r) @?= (Warn, "2 entries")
    ]
  where
    run k = do
      mgr <- newManager defaultManagerSettings
      runProbe mgr (Probe "t" "test" 60 1 k)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = bracket make removeDirectoryRecursive
  where
    make = do
      tmp <- getTemporaryDirectory
      (path, h) <- openTempFile tmp "shiki-test"
      hClose h
      removeFile path
      createDirectory path
      pure path
