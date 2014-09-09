-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module MixKenallGeocode.CommonType
(
  CountryName
, StateName
, CityName
, AreaName
, Latitude
, Longitude
, PostalCode
)
where

type CountryName = String
type StateName = String
type CityName = String
type AreaName = String
type Latitude = Double
type Longitude = Double
type PostalCode = String
