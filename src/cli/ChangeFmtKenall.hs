-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module Main where

import MixKenallGeocode.Csv
import qualified Data.List as L

main :: IO()
main = do
  str <- getContents
  mapM_ (putStr . (++ "\r\n") . L.concat . (L.intersperse ",") . chgCsvRwoFmt) $ parseCsv str
  where
    chgCsvRwoFmt :: CsvRow -> CsvRow
    chgCsvRwoFmt r = [postalCode, stateName, cityName, areaName, "", ""]
      where
        postalCode = r !! 2
        stateName =  r !! 6
        cityName = r !! 7
        areaName = r !! 8
