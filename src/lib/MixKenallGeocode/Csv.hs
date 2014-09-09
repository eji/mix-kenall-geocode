-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}

module MixKenallGeocode.Csv
(
  Csv,
  CsvRow,
  csv,
  withCsv
)
where

import System.IO
import Control.Applicative
import qualified Data.Text    as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Text as P

-- 毎回 try するのが面倒なので、演算子を定義
(<#>) :: (P.ParsecT s u m a) -> (P.ParsecT s u m a) -> (P.ParsecT s u m a)
(<#>) a b = P.try a <|> b
infixl 3 <#>

type CsvParser = P.Parsec T.Text ()

type Csv = [CsvRow]
type CsvRow = [String]

csv :: CsvParser Csv
csv = P.many csvRow

csvRow :: CsvParser CsvRow
csvRow = csvField `P.sepBy1` sep <* eol

csvField :: CsvParser String
csvField = escapedField <#> (P.many normalText)

escapedField :: CsvParser String
escapedField = esc *> _field <* esc
  where
    _field = P.many $
      (esc >> esc)
      <#> sep
      <#> cr
      <#> lf
      <#> normalText

normalText :: CsvParser Char
normalText = P.noneOf ['"', ',', '\r', '\n']

-- CSVの区切り文字
sep :: CsvParser Char
sep = P.char ','

eol :: CsvParser Char
eol = cr >> lf

cr :: CsvParser Char
cr = P.char '\r'

lf :: CsvParser Char
lf = P.char '\n'

-- | CSVのエスケープ文字
esc :: CsvParser Char
esc = P.char '"'

withCsv :: FilePath -> (Csv -> IO a) -> IO()
withCsv path task = withContents path $ \txt ->
  task (parseCsv txt) >> return ()
  where
    parseCsv :: String -> Csv
    parseCsv txt = 
      case P.parse csv "* Parse Error *" (T.pack txt) of
        Left err -> error $ show err
        Right x -> x

withContents :: FilePath -> (String -> IO a) -> IO()
withContents path task = withFile path ReadMode $ \h ->
  hGetContents h >>= task >> return ()
