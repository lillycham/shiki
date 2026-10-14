{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Running probes. Each kind has an IO part that collects facts, and a pure
-- part that judges them, so the judging can be tested without a machine.
module Shiki.Probe
  ( runProbe,

    -- * Judges
    LaunchdInfo (LaunchdInfo, ldRunning, ldPid, ldLastExit, ldRuns),
    parseLaunchd,
    judgeLaunchd,
    judgeHttp,
    judgeAge,
    judgeCount,
    judgeGit,
    judgeExit,
    judgeJsonArray,
    ago,
  )
where

import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.Aeson (Value (Array, Object, String), eitherDecodeStrict, encode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTime)
import Network.HTTP.Client (HttpException, Manager, httpNoBody, parseRequest, responseStatus)
import Network.HTTP.Types (statusCode)
import Shiki.Types (Kind (Command, DirCount, FileAge, Git, Http, Launchd), Output (ExitStatus, JsonArray), Probe (probeGroup, probeKind, probeName, probeTimeout), Result (Result), State (Fail, Ok, Unknown, Warn), Verdict, worst)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.Posix.User (getRealUserID)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | Run one probe within its timeout. It never throws (except to stop its
-- thread): an error is a failed result that says what went wrong.
runProbe :: Manager -> Probe -> IO Result
runProbe mgr p = do
  r <- try (timeout (probeTimeout p * 1000000) (check mgr (probeKind p)))
  now <- getCurrentTime
  (st, summary, detail) <- case r of
    Left e
      | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
      | otherwise -> pure (Fail, "error", Just (T.pack (displayException (e :: SomeException))))
    Right Nothing -> pure (Fail, "timed out after " <> tshow (probeTimeout p) <> " s", Nothing)
    Right (Just v) -> pure v
  pure (Result (probeName p) (probeGroup p) st summary detail (Just now))

check :: Manager -> Kind -> IO Verdict
check _ (Launchd label running) = do
  uid <- getRealUserID
  (code, out, err) <- readProcessWithExitCode "/bin/launchctl" ["print", "gui/" ++ show uid ++ "/" ++ T.unpack label] ""
  pure $ case code of
    ExitSuccess -> judgeLaunchd running (parseLaunchd (T.pack out))
    ExitFailure _ -> (Fail, "not loaded", nonEmpty (T.pack err))
check mgr (Http url expect) = do
  req <- parseRequest url
  t0 <- getMonotonicTime
  r <- try (httpNoBody req mgr)
  t1 <- getMonotonicTime
  pure $ case r of
    Left (e :: HttpException) -> (Fail, "not answering", Just (T.pack (displayException e)))
    Right res -> judgeHttp expect (statusCode (responseStatus res)) (round ((t1 - t0) * 1000))
check _ (FileAge path warnAfter failAfter) = do
  isDir <- doesDirectoryExist path
  isFile <- doesFileExist path
  now <- getCurrentTime
  t <-
    if isDir
      then newest path
      else if isFile then Just <$> getModificationTime path else pure Nothing
  pure $ case t of
    Nothing -> (Fail, if isDir then "empty" else "missing", Just (T.pack path))
    Just m -> judgeAge warnAfter failAfter (diffUTCTime now m)
check _ (DirCount path warnAbove failAbove) =
  judgeCount warnAbove failAbove . length <$> visibleEntries path
check _ (Git path warnAfter upstream) = do
  out <- git path ["log", "-1", "--format=%ct"]
  now <- getCurrentTime
  case out >>= maybe (Left "no commits") Right . readMaybe . T.unpack . T.strip of
    Left e -> pure (Fail, "not a repository", Just e)
    Right (secs :: Integer) -> do
      ahead <-
        if upstream
          then fmap (fmap (fromMaybe 0 . readMaybe . T.unpack . T.strip)) (git path ["rev-list", "--count", "@{upstream}..HEAD"])
          else pure (Right 0)
      let age = diffUTCTime now (posixSecondsToUTCTime (fromIntegral secs))
      pure (judgeGit warnAfter age (if upstream then Just ahead else Nothing))
check _ (Command [] _) = pure (Fail, "empty command", Nothing)
check _ (Command (cmd : args) output) = do
  (code, out, err) <- readProcessWithExitCode cmd args ""
  pure $ case output of
    ExitStatus -> judgeExit code (T.pack out) (T.pack err)
    JsonArray fw ww cb -> case code of
      ExitFailure n -> (Fail, "exit " <> tshow n, nonEmpty (T.pack err))
      ExitSuccess -> case eitherDecodeStrict (TE.encodeUtf8 (T.pack out)) of
        Left e -> (Fail, "not JSON", Just (T.pack e))
        Right v -> judgeJsonArray fw ww cb v

-- | Run git in a repository: its standard output, or its error.
git :: FilePath -> [String] -> IO (Either Text Text)
git path args = do
  (code, out, err) <- readProcessWithExitCode "git" ("-C" : path : args) ""
  pure $ case code of
    ExitSuccess -> Right (T.pack out)
    ExitFailure _ -> Left (T.strip (T.pack err))

visibleEntries :: FilePath -> IO [FilePath]
visibleEntries dir = filter (not . ("." `isPrefixOf`)) <$> listDirectory dir

newest :: FilePath -> IO (Maybe UTCTime)
newest dir = do
  names <- visibleEntries dir
  case names of
    [] -> pure Nothing
    _ -> Just . maximum <$> mapM (getModificationTime . (dir </>)) names

-- Judges ---------------------------------------------------------------------

-- | The top-level fields of @launchctl print@ that a probe needs.
data LaunchdInfo = LaunchdInfo
  { ldRunning :: Bool,
    ldPid :: Maybe Int,
    -- | As launchctl prints it: a number, or "(never exited)".
    ldLastExit :: Maybe Text,
    ldRuns :: Maybe Int
  }
  deriving (Eq, Show)

-- | Read the output of @launchctl print gui/UID/LABEL@. Only the top-level
-- lines count (one tab in): nested sections have keys of the same names.
parseLaunchd :: Text -> LaunchdInfo
parseLaunchd out =
  LaunchdInfo
    { ldRunning = look "state" == Just "running",
      ldPid = readMaybe . T.unpack =<< look "pid",
      ldLastExit = look "last exit code",
      ldRuns = readMaybe . T.unpack =<< look "runs"
    }
  where
    top =
      [ (T.strip k, T.drop 3 v)
      | l <- T.lines out,
        Just rest <- [T.stripPrefix "\t" l],
        not ("\t" `T.isPrefixOf` rest),
        let (k, v) = T.breakOn " = " rest,
        not (T.null v)
      ]
    look k = lookup k top

-- | A job fails when its last exit was not 0, or when it should run and does
-- not. A job that never exited is fine.
judgeLaunchd :: Bool -> LaunchdInfo -> Verdict
judgeLaunchd mustRun i
  | badExit = (Fail, "last exit " <> fromMaybe "?" (ldLastExit i), Nothing)
  | mustRun && not (ldRunning i) = (Fail, "not running", Nothing)
  | ldRunning i = (Ok, "running" <> maybe "" ((" · pid " <>) . tshow) (ldPid i), Nothing)
  | otherwise = (Ok, T.intercalate " · " (["idle"] ++ exitPart ++ runsPart), Nothing)
  where
    badExit = case ldLastExit i of
      Just e -> not (e == "0" || "(never" `T.isPrefixOf` e)
      Nothing -> False
    exitPart = case ldLastExit i of
      Just "0" -> ["exit 0"]
      _ -> []
    runsPart = maybe [] (\n -> [plural n "run"]) (ldRuns i)

judgeHttp :: Int -> Int -> Int -> Verdict
judgeHttp expect code ms
  | code == expect = (Ok, tshow code <> " · " <> tshow ms <> " ms", Nothing)
  | otherwise = (Fail, "status " <> tshow code <> ", wanted " <> tshow expect, Nothing)

-- | Warn when older than the warn age, fail when older than the fail age.
judgeAge :: Int -> Maybe Int -> NominalDiffTime -> Verdict
judgeAge warnAfter failAfter age = (st, ago age, Nothing)
  where
    older s = age > fromIntegral s
    st
      | maybe False older failAfter = Fail
      | older warnAfter = Warn
      | otherwise = Ok

-- | Warn above the warn count, fail above the fail count.
judgeCount :: Int -> Maybe Int -> Int -> Verdict
judgeCount warnAbove failAbove n = (st, plural n "entry", Nothing)
  where
    st
      | maybe False (n >) failAbove = Fail
      | n > warnAbove = Warn
      | otherwise = Ok

-- | The age of the last commit, and how many commits are not pushed
-- ('Nothing' when the probe does not look at the upstream).
judgeGit :: Maybe Int -> NominalDiffTime -> Maybe (Either Text Int) -> Verdict
judgeGit warnAfter age ahead = (worst (map fst parts), T.intercalate " · " (map snd parts), detail)
  where
    agePart = (if maybe False (\s -> age > fromIntegral s) warnAfter then Warn else Ok, "last commit " <> ago age)
    pushPart = case ahead of
      Nothing -> []
      Just (Right 0) -> [(Ok, "pushed")]
      Just (Right n) -> [(Warn, plural n "commit" <> " not pushed")]
      Just (Left _) -> [(Warn, "no upstream")]
    parts = agePart : pushPart
    detail = case ahead of
      Just (Left e) | not (T.null e) -> Just e
      _ -> Nothing

judgeExit :: ExitCode -> Text -> Text -> Verdict
judgeExit ExitSuccess out _ = (Ok, fromMaybe "exit 0" (lastLine out), Nothing)
judgeExit (ExitFailure n) out err = (Fail, "exit " <> tshow n, lastLine err <> lastLine out)

-- | Judge a JSON array by the fields of its elements.
judgeJsonArray :: Map Text Value -> Map Text Value -> Maybe Text -> Value -> Verdict
judgeJsonArray fw ww cb (Array xs) = (st, summary, Nothing)
  where
    items = V.toList xs
    has w x =
      not (M.null w) && case x of
        Object o -> all (\(k, v) -> KM.lookup (K.fromText k) o == Just v) (M.toList w)
        _ -> False
    st
      | any (has fw) items = Fail
      | any (has ww) items = Warn
      | otherwise = Ok
    summary
      | null items = "none"
      | Just field <- cb = T.intercalate " · " [tshow n <> " " <> k | (k, n) <- sortOn (Down . snd) (M.toList (countField field))]
      | otherwise = plural (length items) "item"
    countField field = M.fromListWith (+) [(fieldText field x, 1 :: Int) | x <- items]
    fieldText field = \case
      Object o -> maybe "?" valueText (KM.lookup (K.fromText field) o)
      _ -> "?"
judgeJsonArray _ _ _ _ = (Unknown, "not a JSON array", Nothing)

-- | A JSON value as a short label: a string without its quotes.
valueText :: Value -> Text
valueText = \case
  String s -> s
  v -> TE.decodeUtf8 (BL.toStrict (encode v))

-- Helpers --------------------------------------------------------------------

-- | A short age: "just now", "5m ago", "3h 12m ago", "2d ago".
ago :: NominalDiffTime -> Text
ago age
  | s < 60 = "just now"
  | s < 3600 = tshow (s `div` 60) <> "m ago"
  | s < 48 * 3600 = tshow (s `div` 3600) <> "h " <> tshow ((s `mod` 3600) `div` 60) <> "m ago"
  | otherwise = tshow (s `div` 86400) <> "d ago"
  where
    s = max 0 (floor age) :: Int

plural :: Int -> Text -> Text
plural 1 w = "1 " <> w
plural n w
  | "y" `T.isSuffixOf` w = tshow n <> " " <> T.dropEnd 1 w <> "ies"
  | otherwise = tshow n <> " " <> w <> "s"

lastLine :: Text -> Maybe Text
lastLine = nonEmpty . last' . filter (not . T.null . T.strip) . T.lines
  where
    last' [] = ""
    last' ls = T.strip (last ls)

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null (T.strip t) then Nothing else Just (T.strip t)

tshow :: (Show a) => a -> Text
tshow = T.pack . show
