-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module MixKenallGeocode.Geocode
(
  csvToStateGeoInfo,
  StateGeoInfo,
  CityGeoInfo,
  AreaGeoInfo
)
where

import qualified Data.HashTable.IO as H
import MixKenallGeocode.Csv
import MixKenallGeocode.CommonType

type HashTable k v = H.BasicHashTable k v

type StateGeoInfo = HashTable StateName CityGeoInfo
type CityGeoInfo = HashTable CityName [AreaGeoInfo]
type AreaGeoInfo = (AreaName, Latitude, Longitude)

csvToStateGeoInfo :: Csv -> IO(StateGeoInfo)
csvToStateGeoInfo csv = do
  ht <- H.new
  csvToStateGeoInfoAux ht csv

csvToStateGeoInfoAux :: StateGeoInfo -> Csv -> IO(StateGeoInfo)
csvToStateGeoInfoAux s [] = return s
csvToStateGeoInfoAux s (x:xs) = do
  r1 <- H.lookup s stateNm
  ci <- case r1 of
          Just x -> return x
          Nothing -> H.new
  r2 <- H.lookup ci cityNm
  ai <- case r2 of
          Just x -> return x
          Nothing -> return []
  H.insert ci cityNm $ (areaNm, lat, lon):ai
  H.insert s stateNm ci
  csvToStateGeoInfoAux s xs
  where
    stateNm :: StateName
    stateNm = x !! 0
    cityNm :: CityName
    cityNm = x !! 1
    areaNm :: AreaName
    areaNm = x !! 2
    lat :: Latitude
    lat = read (x !! 7) :: Latitude
    lon :: Longitude
    lon = read (x !! 8) :: Longitude
