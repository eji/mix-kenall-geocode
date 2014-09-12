-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module MixKenallGeocode.Util
where

import Control.Applicative
import qualified Text.Parsec as P

-- 毎回 try するのが面倒なので、演算子を定義
(<#>) :: (P.ParsecT s u m a) -> (P.ParsecT s u m a) -> (P.ParsecT s u m a)
(<#>) a b = P.try a <|> b
infixl 3 <#>

fullWidthNumToInt :: String -> Int
fullWidthNumToInt s = read (map fullToHalfWidthNum s) :: Int

fullToHalfWidthNum :: Char -> Char
fullToHalfWidthNum '０' = '0'
fullToHalfWidthNum '１' = '1'
fullToHalfWidthNum '２' = '2'
fullToHalfWidthNum '３' = '3'
fullToHalfWidthNum '４' = '4'
fullToHalfWidthNum '５' = '5'
fullToHalfWidthNum '６' = '6'
fullToHalfWidthNum '７' = '7'
fullToHalfWidthNum '８' = '8'
fullToHalfWidthNum '９' = '9'
fullToHalfWidthNum _ = error "unexpected char"
