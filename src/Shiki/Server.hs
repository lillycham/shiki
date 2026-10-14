{-# LANGUAGE OverloadedStrings #-}

-- | Run the probes on their timers, and serve their last results as JSON.
--
-- A request never runs a probe: it reads what the timers left in memory, so
-- a slow probe never makes a page wait.
module Shiki.Server
  ( serve,
    checkOnce,
    app,
    overview,
    groupJson,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_, race_)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (forever)
import Data.Aeson (ToJSON, Value, encode, object, (.=))
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status, hContentType, status200, status404, status405)
import Network.Wai (Application, ResponseReceived, pathInfo, requestMethod, responseLBS)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import Shiki.Probe (runProbe)
import Shiki.Types (Config (cfgHost, cfgPort, cfgProbes), Probe (probeEvery, probeName), Result (resGroup, resState), pending, stateText, worst)

-- | Run every probe on its own timer, and serve the results.
serve :: Config -> IO ()
serve cfg = do
  mgr <- newTlsManager
  store <- newTVarIO (M.fromList [(probeName p, pending p) | p <- probes])
  let loop (i, p) = do
        -- Start the probes a little apart, so they do not all run at once.
        threadDelay (i * 250000)
        forever $ do
          r <- runProbe mgr p
          atomically (modifyTVar' store (M.insert (probeName p) r))
          threadDelay (probeEvery p * 1000000)
      settings = setHost (fromString (cfgHost cfg)) (setPort (cfgPort cfg) defaultSettings)
  race_
    (mapConcurrently_ loop (zip [0 ..] probes))
    (runSettings settings (app (map probeName probes) store))
  where
    probes = cfgProbes cfg

-- | Run every probe once, at the same time, and return the results in the
-- order of the config.
checkOnce :: Config -> IO [Result]
checkOnce cfg = do
  mgr <- newTlsManager
  mapConcurrently (runProbe mgr) (cfgProbes cfg)

-- | The HTTP side. @order@ is the probe names in the order of the config.
--
-- * @GET /probes@: every group and probe.
-- * @GET /probes/GROUP@: one group.
-- * @GET /health@: "ok", for a monitor.
app :: [Text] -> TVar (Map Text Result) -> Application
app order store req respond = case (requestMethod req, pathInfo req) of
  ("GET", ["probes"]) -> snapshot >>= json status200 . overview
  ("GET", ["probes", g]) ->
    snapshot >>= \rs -> case filter ((== g) . resGroup) rs of
      [] -> json status404 (object ["error" .= ("no group named " <> g)])
      xs -> json status200 (groupJson g xs)
  ("GET", ["health"]) -> respond (responseLBS status200 [(hContentType, "text/plain")] "ok\n")
  ("GET", _) -> json status404 (object ["error" .= ("not found; try /probes" :: Text)])
  _ -> json status405 (object ["error" .= ("only GET" :: Text)])
  where
    snapshot = do
      m <- readTVarIO store
      pure (mapMaybe (`M.lookup` m) order)
    json :: (ToJSON a) => Status -> a -> IO ResponseReceived
    json st v = respond (responseLBS st [(hContentType, "application/json")] (encode v))

-- | All the results, by group, with the worst state of each group on top.
overview :: [Result] -> Value
overview rs =
  object
    [ "state" .= stateText (worst (map resState rs)),
      "groups" .= [groupJson g (filter ((== g) . resGroup) rs) | g <- nub (map resGroup rs)],
      "probes" .= rs
    ]

-- | One group: its worst state, a count of its states, and its probes.
groupJson :: Text -> [Result] -> Value
groupJson g rs =
  object
    [ "group" .= g,
      "state" .= stateText (worst (map resState rs)),
      "summary" .= T.intercalate " · " [T.pack (show n) <> " " <> stateText s | (s, n) <- counts, n > 0],
      "probes" .= rs
    ]
  where
    counts = [(s, length (filter ((== s) . resState) rs)) | s <- reverse [minBound .. maxBound]]
