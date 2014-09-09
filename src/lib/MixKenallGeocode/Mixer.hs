-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MixKenallGeocode.Mixer
where

import qualified Text.EditDistance as ED
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import qualified Data.Text as T
import MixKenallGeocode.Csv
import MixKenallGeocode.Geocode
import MixKenallGeocode.KenAllCsv
import MixKenallGeocode.CommonType


data MixedCsvRow =
  MixedCsvRow
  {
    _CountryName :: CountryName
  , _StateName :: StateName
  , _CityName :: CityName
  , _PostalCode :: PostalCode
  , _AreaName :: AreaName
  , _Latitude :: Maybe(Latitude)
  , _Longitude :: Maybe(Longitude)
  }
  deriving (Eq, Show)


mixedCsvRowToText :: MixedCsvRow -> T.Text
mixedCsvRowToText r@(MixedCsvRow {..}) =
  T.intercalate (T.pack ",") 
    [
      T.pack _CountryName
    , T.pack _StateName
    , T.pack _PostalCode
    , T.pack _CityName
    , T.pack _AreaName
    , T.pack lat
    , T.pack lon
    ]
  where
    lat = case _Latitude of
            Just x -> show x
            Nothing -> ""
    lon = case _Longitude of
            Just x -> show x
            Nothing -> ""

countryName = "JAPAN"

mix :: StateGeoInfo -> [SimpleKenAllCsvRow] -> IO([MixedCsvRow])
mix sgi = mapM (mixRow sgi)

mixRow :: StateGeoInfo -> SimpleKenAllCsvRow -> IO(MixedCsvRow)
mixRow sgi row = do
  r <- lookupAreaGeoInfoList sgi (_sName row) (_cName row)
  case r of
    Nothing -> return $ MixedCsvRow countryName (_sName row) (_cName row) (_pCode row) (_aName row) Nothing Nothing
    Just x -> 
      case findMinEditDistanceAreaName x (_aName row) of
        Nothing -> return $ MixedCsvRow countryName (_sName row) (_cName row) (_pCode row) (_aName row) Nothing Nothing
        Just (_, lat, lon) -> return $ MixedCsvRow countryName (_sName row) (_cName row) (_pCode row) (_aName row) (Just lat) (Just lon)

lookupAreaGeoInfoList :: StateGeoInfo -> StateName -> CityName -> IO(Maybe([AreaGeoInfo]))
lookupAreaGeoInfoList t sn cn = do
  r <- H.lookup t sn
  case r of
    Nothing -> return Nothing
    Just x -> H.lookup x cn


findMinEditDistanceAreaName :: [AreaGeoInfo] -> AreaName -> Maybe(AreaGeoInfo)
findMinEditDistanceAreaName [] _ = Nothing
findMinEditDistanceAreaName ax n =
  Just $ fst $ L.minimumBy cmp [(a, editDist an n) | a@(an, lat, lon) <- ax]
  where
    cmp :: (a, Int) -> (b, Int) -> Ordering
    cmp (_, c1) (_, c2)
      | c1 < c2 = LT
      | otherwise = GT

editDist :: String -> String -> Int
editDist = ED.levenshteinDistance ED.defaultEditCosts 

