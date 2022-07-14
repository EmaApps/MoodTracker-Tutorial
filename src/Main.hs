{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Time
import Ema
import Ema.Route.Generic
import Generics.SOP qualified as SOP
import Optics.Core (prism')

data Route
  = Route_Index
  | Route_Date Date
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ -- This is automatically deduced in GHC 9.2
               -- But nixpkgs is still oin 9.0, so we must manually specify it.
               WithSubRoutes
                '[ FileRoute "index.html"
                 , FolderRoute "date" Date
                 ]
             ]
        )

-- | Isomorphic to `Data.Time.Calendar.Day`
newtype Date = Date (Integer, Int, Int)
  deriving stock
    (Show, Eq, Ord, Generic)

instance IsRoute Date where
  type RouteModel Date = ()
  routeEncoder = mkRouteEncoder $ \() ->
    prism'
      ( \(Date (y, m, d)) ->
          formatTime defaultTimeLocale "%Y-%m-%d.html" $
            fromGregorian y m d
      )
      ( fmap (Date . toGregorian)
          . parseTimeM False defaultTimeLocale "%Y-%m-%d.html"
      )
  allRoutes _ = []

instance EmaSite Route where
  siteInput _ _ = pure $ pure ()
  siteOutput rp () r = Ema.AssetGenerated Ema.Html "TODO"

main :: IO ()
main = Ema.runSite_ @Route ()