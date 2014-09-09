-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}

module MixKenallGeocode.KenAllCsvSpec
where

import SpecHelper
import qualified Text.Parsec as P
import MixKenallGeocode.KenAllCsv

spec :: Spec
spec = do
  describe "parseAreaOption" $ do
    context "オプションがない場合" $ do
      it "処理されないこと" $
        let area = AreaNameNoOpt "XX"
            row = KenAllCsvRow "0337777" "北海道" "札幌市"  area
        in parseAreaOption row `shouldBe` row
      
    context "閉じているオプションがある場合" $ do
      it "処理されること" $
        let area = AreaNameOpt "XX" "AA"
            parea = AreaNameParsedOpt "XX" [AreaOptAddr (AreaRangeNumOnly  (AreaNumNm "AA" NoNum "" []))]
            row = KenAllCsvRow "0337777" "北海道" "札幌市"  area
            expect = KenAllCsvRow "0337777" "北海道" "札幌市"  parea
        in parseAreaOption row `shouldBe` expect

  describe "areaPrefix" $ do
    context "空文字列の場合" $ do
      it "パースできること" $ do
        (P.parse areaPrefix "" "") `shouldParse` ""
     
    context "数字を含まない文字列の場合" $ do
      it "パースできること" $ do
        (P.parse areaPrefix "" "テスト") `shouldParse` "テスト"
     
    context "数字を含む文字列の場合" $ do
      it "数字より前の文字列がPrefixとしてパースされること" $ do
        (P.parse areaPrefix "" "テスト３７") `shouldParse` "テスト"
     
    context "ノートの区切り文字を含む文字列の場合" $ do
      it "区切り文字より前の文字列がPrefixとしてパースされること" $ do
        (P.parse areaPrefix "" "テスト「AA」") `shouldParse` "テスト"


  describe "areaUnit" $ do
    it "住所単位をパースできること" $
      (P.parse areaUnit "" "丁目") `shouldParse` "丁目"
    context "住所単位以外の文字の場合" $ do
      it "空文字が返ってくること" $ do
        (P.parse areaUnit "" "AA") `shouldParse` ""
   
  describe "areaNum" $ do
    context "数字の場合" $ do
      it "住所の数値をパースできること" $
        (P.parse areaNum "" "１２０") `shouldParse` AreaNum 120

  describe "sepStrs" $ do
    it "パースできること" $
      (P.parse sepStrs "" "「") `shouldParse` "「"
    it "パースできること" $
      (P.parse sepStrs "" "」") `shouldParse` "」"

  describe "areaNote" $ do
    it "パースできること" $
      (P.parse areaNote "" "「AA」") `shouldParse` AreaRawNote "AA"
