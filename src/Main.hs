{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Exception (throw)
import Control.Monad.Logger (logErrorNS, logInfoNS)
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Time
import Ema
import Ema.Route.Generic.TH
import GHC.IO.Exception (userError)
import Optics.Core (prism')
import System.FSNotify qualified as FSNotify
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import UnliftIO (Chan, newChan, readChan)
import UnliftIO.Concurrent (forkIO, threadDelay)

data Model = Model
  { modelDays :: Map Date Mood
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | Isomorphic to `Data.Time.Calendar.Day`
newtype Date = Date (Integer, Int, Int)
  deriving stock (Show, Eq, Ord, Generic)

data Mood = Bad | Neutral | Good
  deriving stock (Show, Eq, Ord, Generic, Read)

data Route
  = Route_Index
  | Route_Date Date
  deriving stock (Show, Eq, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute ''Route [t|'[WithModel Model]|]

instance IsRoute Date where
  type RouteModel Date = Model
  routePrism (Model _moods) =
    toPrism_ $
      prism'
        ( \(Date (y, m, d)) ->
            formatTime defaultTimeLocale "%Y-%m-%d.html" $
              fromGregorian y m d
        )
        ( fmap (Date . toGregorian)
            . parseTimeM False defaultTimeLocale "%Y-%m-%d.html"
        )
  routeUniverse (Model moods) = Map.keys moods

instance Csv.FromField Date where
  parseField f = do
    s <- Csv.parseField @String f
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" s of
      Left err -> fail err
      Right date ->
        pure $ Date $ toGregorian date

instance Csv.FromField Mood where
  parseField f = do
    s <- Csv.parseField @String f
    case readEither @Mood s of
      Left err -> fail $ toString err
      Right v -> pure v

instance EmaSite Route where
  siteInput _ _ = do
    model0 <- readModel "data/moods.csv"
    pure . Dynamic . (model0,) $ \setModel -> do
      ch <- liftIO $ watchDirForked "data"
      let loop = do
            logInfoNS "fsnotify" "Waiting for fs event ..."
            evt <- liftIO $ readChan ch
            logInfoNS "fsnotify" $ "Got fs event: " <> show evt
            setModel =<< readModel (FSNotify.eventPath evt)
            loop
      loop
    where
      readModel fp = do
        s <- readFileLBS fp
        case toList <$> Csv.decode Csv.NoHeader s of
          Left err -> logErrorNS "csv" (toText err) >> pure (Model mempty)
          Right moods -> pure $ Model $ Map.fromList moods
      -- Observe changes to a directory path, and return the `Chan` of its events.
      watchDirForked :: FilePath -> IO (Chan FSNotify.Event)
      watchDirForked path = do
        ch <- newChan
        -- FIXME: We should be using race_, not forkIO.
        void . forkIO $
          FSNotify.withManager $ \mgr -> do
            _stopListening <- FSNotify.watchDirChan mgr path (const True) ch
            threadDelay maxBound
        pure ch

  siteOutput rp model r =
    Ema.AssetGenerated Ema.Html . RU.renderHtml $ do
      H.docType
      H.html ! A.lang "en" $ do
        H.head $ do
          H.title "Mood tracker"
        H.body $ case r of
          Route_Index -> do
            H.h1 "Mood tracker"
            forM_ (Map.toList $ modelDays model) $ \(date, mood) -> do
              H.li $ do
                let url = Ema.routeUrl rp $ Route_Date date
                H.a ! A.href (H.toValue url) $
                  show date
                ": "
                show mood
          Route_Date d -> do
            H.h1 (show d)
            H.pre $ show $ Map.lookup d (modelDays model)

main :: IO ()
main = Ema.runSite_ @Route ()
