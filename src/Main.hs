{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Exception (throw)
import Data.Csv qualified as Csv
import Data.Map.Strict qualified as Map
import Data.Time
import Ema
import Ema.Route.Generic
import GHC.IO.Exception (userError)
import Generics.SOP qualified as SOP
import Optics.Core (prism')
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

data Route
  = Route_Index
  | Route_Date Date
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ WithModel Model
             , -- This is automatically deduced in GHC 9.2
               -- But nixpkgs is still oin 9.0, so we must manually specify it.
               WithSubRoutes
                '[ FileRoute "index.html"
                 , FolderRoute "date" Date
                 ]
             ]
        )

-- | Isomorphic to `Data.Time.Calendar.Day`
newtype Date = Date (Integer, Int, Int)
  deriving stock (Show, Eq, Ord, Generic)

instance Csv.FromField Date where
  parseField f = do
    s <- Csv.parseField @String f
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" s of
      Left err -> fail err
      Right date ->
        pure $ Date $ toGregorian date

instance IsRoute Date where
  type RouteModel Date = Model
  routeEncoder = mkRouteEncoder $ \(Model moods) ->
    prism'
      ( \(Date (y, m, d)) ->
          formatTime defaultTimeLocale "%Y-%m-%d.html" $
            fromGregorian y m d
      )
      ( fmap (Date . toGregorian)
          . parseTimeM False defaultTimeLocale "%Y-%m-%d.html"
      )
  allRoutes (Model moods) = Map.keys moods

data Model = Model
  { modelDays :: Map Date Mood
  }
  deriving stock (Show, Eq, Ord, Generic)

data Mood = Bad | Neutral | Good
  deriving stock (Show, Eq, Ord, Generic, Read)

instance Csv.FromField Mood where
  parseField f = do
    s <- Csv.parseField @String f
    case readEither @Mood s of
      Left err -> fail $ toString err
      Right v -> pure v

instance EmaSite Route where
  siteInput _ _ = do
    s <- readFileLBS "data/moods.csv"
    case toList <$> Csv.decode Csv.NoHeader s of
      Left err -> throw $ userError err
      Right moods ->
        pure $ pure $ Model $ Map.fromList moods
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