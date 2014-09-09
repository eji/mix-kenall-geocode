-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module MixKenallGeocode.KenAllCsvSpec
where

import SpecHelper
import MixKenallGeocode.KenAllCsv

spec :: Spec
spec = do
  describe "parse areaUnit" $ do
    it "hoge" $
      normalizeKenAllCsv [] `shouldBe` []


