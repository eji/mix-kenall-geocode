-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import MixKenallGeocode
import qualified Data.Text.IO as T

main :: IO()
main = do
  optsInfo <- execParser opts
  withCsv (_geoCsv optsInfo) $ \geoCsv -> do
    si <- csvToStateGeoInfo geoCsv
    withCsv (_kenAllCsv optsInfo) $ \kaCsv -> do
      mixedCsv <- mix si $ normalizeKenAllCsv kaCsv
      mapM (T.putStrLn . mixedCsvRowToText) mixedCsv

{- オプション -}
data Options = Options
  { _kenAllCsv :: FilePath
  , _geoCsv :: FilePath
  }
  deriving Show

options :: Parser Options
options = Options
  <$> strOption (short 'k' <> long "ken-all-csv" <> metavar "PATH" <> help "KEN_ALL.CSVのパス")
  <*> strOption (short 'g' <> long "geo-csv" <> metavar "PATH" <> help "国土交通省の位置情報が記載されたCSVのパス")

opts :: ParserInfo Options
opts =
  info
    (helper <*> options)
    (
      fullDesc
      <> progDesc "出力フォーマット: 国名,都道府県名,市町村名,郵便番号,緯度,経度"
      <> header "Tableauに郵便番号と位置情報を登録するためのCSVを生成するたえのコマンド"
    )

