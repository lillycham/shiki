{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What shiki checks, and what it found.
module Shiki.Types
  ( State (Ok, Unknown, Warn, Fail),
    stateText,
    worst,
    Verdict,
    Result (Result, resName, resGroup, resState, resSummary, resDetail, resChecked),
    pending,
    Probe (Probe, probeName, probeGroup, probeEvery, probeTimeout, probeKind),
    Kind (Launchd, Http, FileAge, DirCount, Git, Command),
    Output (ExitStatus, JsonArray),
    Config (Config, cfgHost, cfgPort, cfgProbes),
    loadConfig,
    validate,
  )
where

import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value, eitherDecodeFileStrict, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.List (group, sort)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)

-- | How a probe found its thing. The order is how bad it is, so a group
-- shows the worst state of its probes. A probe that has not run yet, or
-- could not tell, is 'Unknown': worse than 'Ok', better than 'Warn'.
data State = Ok | Unknown | Warn | Fail
  deriving (Eq, Ord, Show, Enum, Bounded)

stateText :: State -> Text
stateText = \case
  Ok -> "ok"
  Unknown -> "unknown"
  Warn -> "warn"
  Fail -> "fail"

-- | The worst of some states. No states at all is 'Unknown'.
worst :: [State] -> State
worst [] = Unknown
worst ss = maximum ss

-- | What a check decides: a state, a one-line summary, and maybe more detail.
type Verdict = (State, Text, Maybe Text)

-- | The last result of one probe.
data Result = Result
  { resName :: Text,
    resGroup :: Text,
    resState :: State,
    resSummary :: Text,
    resDetail :: Maybe Text,
    resChecked :: Maybe UTCTime
  }
  deriving (Eq, Show)

instance ToJSON Result where
  toJSON r =
    object
      [ "name" .= resName r,
        "group" .= resGroup r,
        "state" .= stateText (resState r),
        "summary" .= resSummary r,
        "detail" .= resDetail r,
        "checked" .= resChecked r
      ]

-- | The result of a probe before its first run.
pending :: Probe -> Result
pending p = Result (probeName p) (probeGroup p) Unknown "not checked yet" Nothing Nothing

-- | One thing to check, and how often.
data Probe = Probe
  { probeName :: Text,
    probeGroup :: Text,
    -- | Seconds between runs.
    probeEvery :: Int,
    -- | Seconds a run may take.
    probeTimeout :: Int,
    probeKind :: Kind
  }
  deriving (Eq, Show)

-- | The kinds of check. Durations are in seconds.
data Kind
  = -- | A launchd job of the user: its last exit code, and whether it runs.
    Launchd {launchdLabel :: Text, launchdRunning :: Bool}
  | -- | An HTTP request that must get the expected status.
    Http {httpUrl :: String, httpExpect :: Int}
  | -- | The age of a file, or of the newest entry of a directory.
    FileAge {agePath :: FilePath, ageWarn :: Int, ageFail :: Maybe Int}
  | -- | The number of entries of a directory (dot files left out).
    DirCount {countPath :: FilePath, countWarn :: Int, countFail :: Maybe Int}
  | -- | The age of the last commit of a git repository, and whether it is pushed.
    Git {gitPath :: FilePath, gitWarn :: Maybe Int, gitUpstream :: Bool}
  | -- | A fixed command from the config, never from a request.
    Command {cmdArgv :: [String], cmdOutput :: Output}
  deriving (Eq, Show)

-- | How to read the output of a command.
data Output
  = -- | Exit code 0 is ok, any other code fails.
    ExitStatus
  | -- | Standard output is a JSON array. An element that has all the fields
    -- of the fail map fails the probe; one that has all the fields of the
    -- warn map warns. With a count-by field, the summary counts the
    -- elements by the value of that field.
    JsonArray {failWhere :: Map Text Value, warnWhere :: Map Text Value, countBy :: Maybe Text}
  deriving (Eq, Show)

instance FromJSON Probe where
  parseJSON = withObject "probe" $ \o -> do
    name <- o .: "name"
    grp <- o .:? "group" .!= "default"
    every <- o .:? "every" .!= 60
    tmo <- o .:? "timeout" .!= 10
    kind <- o .: "kind" >>= kindFrom o
    pure (Probe name grp every tmo kind)

kindFrom :: Object -> Text -> Parser Kind
kindFrom o = \case
  "launchd" -> Launchd <$> o .: "label" <*> o .:? "running" .!= False
  "http" -> Http <$> o .: "url" <*> o .:? "expect" .!= 200
  "file-age" -> FileAge <$> o .: "path" <*> o .: "warn-after" <*> o .:? "fail-after"
  "dir-count" -> DirCount <$> o .: "path" <*> o .: "warn-above" <*> o .:? "fail-above"
  "git" -> Git <$> o .: "path" <*> o .:? "warn-after" <*> o .:? "upstream" .!= True
  "command" -> do
    argv <- o .: "argv"
    if null argv then fail "a command needs a non-empty argv" else Command argv <$> outputFrom o
  other -> fail ("unknown kind " ++ show other ++ "; the kinds are launchd, http, file-age, dir-count, git and command")

outputFrom :: Object -> Parser Output
outputFrom o =
  o .:? "output" .!= ("exit" :: Text) >>= \case
    "exit" -> pure ExitStatus
    "json-array" -> JsonArray <$> o .:? "fail-where" .!= mempty <*> o .:? "warn-where" .!= mempty <*> o .:? "count-by"
    other -> fail ("unknown output " ++ show other ++ "; use exit or json-array")

-- | Where to listen, and what to check.
data Config = Config
  { cfgHost :: String,
    cfgPort :: Int,
    cfgProbes :: [Probe]
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o ->
    Config
      <$> o .:? "host" .!= "127.0.0.1"
      <*> o .:? "port" .!= 8092
      <*> o .: "probes"

-- | Problems that the JSON parser cannot see: names must be unique, and
-- times must be positive.
validate :: Config -> Either String Config
validate c
  | (d : _) <- dups = Left ("two probes are named " ++ show d)
  | (p : _) <- filter (\p -> probeEvery p < 1 || probeTimeout p < 1) (cfgProbes c) =
      Left ("probe " ++ show (probeName p) ++ ": every and timeout must be at least 1 second")
  | otherwise = Right c
  where
    dups = [T.unpack n | (n : _ : _) <- group (sort (map probeName (cfgProbes c)))]

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = (>>= validate) <$> eitherDecodeFileStrict path
