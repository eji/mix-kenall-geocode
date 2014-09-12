-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.EditDistance as ED
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P
import qualified Text.Read as R
import MixKenallGeocode.Csv
import MixKenallGeocode.Util


main :: IO()
main = do
  optsInfo <- execParser opts
  kenAllStr <- getContents
  withCsv (_geoCsv optsInfo) $ \geoCsv -> do
    mapM_ (\x -> T.putStr x >> putStr "\r\n")
        $ map mixedCsvRowToText
        $ mix (csvToAreaGeoInfo geoCsv)
        $ normalizeKenAllCsv
        $ parseCsv kenAllStr

{- オプション -}
data Options = Options { _geoCsv :: FilePath }
  deriving Show

options :: Parser Options
options = Options
  <$> strOption (short 'g' <> long "geo-csv" <> metavar "PATH" <> help "国土交通省の位置情報が記載されたCSVのパス")

opts :: ParserInfo Options
opts =
  info
    (helper <*> options)
    (
      fullDesc
      <> progDesc "出力フォーマット: 都道府県名,市町村名,郵便番号,緯度,経度"
      <> header "Tableauに郵便番号と位置情報を登録するためのCSVを生成するたえのコマンド"
    )

{- 型定義 -}
type StateName = String
type CityName = String
type AreaName = String
type Latitude = Double
type Longitude = Double
type PostalCode = String


data NormalizedCsvRow =
  NormalizedCsvRow
  {
    _StateName :: StateName
  , _CityName :: CityName
  , _PostalCode :: PostalCode
  , _AreaName :: AreaName
  , _Latitude :: Maybe(Latitude)
  , _Longitude :: Maybe(Longitude)
  }
  deriving (Eq, Show)


data KenAllCsvRow = KenAllCsvRow
  { _postalCode :: PostalCode
  , _stateName :: StateName
  , _cityName :: CityName
  , _areaName :: ParsedAreaName
  , _latitude :: Maybe Latitude
  , _longitude :: Maybe Longitude
  }
  deriving (Show, Eq)

type AreaOptionParser = P.Parsec T.Text ()

-- 町域名
data ParsedAreaName =
  AreaNameNoOpt String
  | AreaNameOpt String String
  | AreaNameBeginOpt String String
  | AreaNameEndOpt String
  | AreaNameParsedOpt String [AreaOptAddr]
  deriving (Show, Eq)

data AreaOptAddr = 
  AreaOptAddr AreaRange
  | AreaOptAddrWithBranch { _left :: AreaRange, _right :: AreaRange }
  deriving (Eq, Show)
  
data AreaRange =
  AreaRangeNumOnly AreaNumNm
  | AreaRange { _from :: AreaNumNm, _to :: AreaNumNm }
  deriving (Eq, Show)

data AreaNumNm = 
  NoareaNumNm
  | AreaNumNm
    { _prefix :: String
    , _num :: AreaNum
    , _unit :: String
    , _notes :: [AreaNote]
    }
  deriving (Eq, Show)

data AreaNum = AreaNum Int | NoNum
  deriving (Eq, Show)

data AreaNote =
  AreaRawNote String
  | AreaParsedNote ParsedNote
  deriving (Eq, Show)

-- data ParsedNote =
--   ExceptionNote [AreaOptAddr]
--   | ParsedNote [AreaOptAddr]
--   deriving (Eq, Show)
data ParsedNote =
  ExceptionNote String
  | ParsedNote String
  deriving (Eq, Show)

parsedAreaName :: P.Parser ParsedAreaName
parsedAreaName =
  areaNameOpt
  <#> areaNameBeginOpt
  <#> areaNameEndOpt
  <#> areaNameNoOpt

notOptionField :: P.Parser String
notOptionField = P.many $ P.noneOf "（）"

areaNameNoOpt :: P.Parser ParsedAreaName
areaNameNoOpt = AreaNameNoOpt <$> notOptionField

areaNameOpt :: P.Parser ParsedAreaName
areaNameOpt = do
  areaNm <- notOptionField
  opt <- (P.char '（') *> notOptionField <* (P.char '）')
  return $ AreaNameOpt areaNm opt

areaNameBeginOpt :: P.Parser ParsedAreaName
areaNameBeginOpt = do
  areaNm <- notOptionField
  opt <- (P.char '（') *> notOptionField
  return $ AreaNameBeginOpt areaNm opt

areaNameEndOpt :: P.Parser ParsedAreaName
areaNameEndOpt = do
  opt <- notOptionField <* (P.char '）')
  return $ AreaNameEndOpt opt


reduceKenAllCsv :: Csv -> [KenAllCsvRow]
reduceKenAllCsv = map reduceKenAllCsvRow 

reduceKenAllCsvRow :: CsvRow -> KenAllCsvRow
reduceKenAllCsvRow r =
  KenAllCsvRow
  { _postalCode = r !! 0
  , _stateName =  r !! 1
  , _cityName = r !! 2
  , _areaName = parsedArNm
  , _latitude = (R.readMaybe (r !! 4)) :: Maybe Latitude
  , _longitude = (R.readMaybe (r !! 5)) :: Maybe Longitude
  }
  where
    arNm = r!!3
    parsedArNm =
      case P.parse parsedAreaName "* Parse Error *" (T.pack arNm) of
        Left err -> error $ show err
        Right x -> x

{- 複数行に渡る行をマージ -}

margeRows :: [KenAllCsvRow] -> [KenAllCsvRow]
margeRows [] = []
margeRows (r:rs) =
  case (_areaName r) of
    AreaNameBeginOpt _ _ ->
      let (mr, rest) = splitMargedRowAndRest r rs
      in mr:(margeRows rest)
    _ -> r:(margeRows rs)

splitMargedRowAndRest :: KenAllCsvRow -> [KenAllCsvRow] -> (KenAllCsvRow, [KenAllCsvRow])
splitMargedRowAndRest r rs = (r{_areaName = marged_r}, tail rest)
  where
    (xs, rest) = break hasAreaNameEndOpt rs
    marged_r = margeParsedAreaNames (_areaName r) $ map _areaName $ xs ++ [head rest]
    hasAreaNameEndOpt :: KenAllCsvRow -> Bool
    hasAreaNameEndOpt (KenAllCsvRow {..}) =
      case _areaName of
        (AreaNameEndOpt _) -> True
        _ -> False

margeParsedAreaNames :: ParsedAreaName -> [ParsedAreaName] -> ParsedAreaName
margeParsedAreaNames (AreaNameBeginOpt an opt) as = AreaNameOpt an $ concat $ opt:(map takeOptStr as)
  where
    takeOptStr :: ParsedAreaName -> String
    takeOptStr (AreaNameNoOpt x) = x
    takeOptStr (AreaNameEndOpt x) = x
    takeOptStr _ = ""
margeParsedAreaNames _ _ = AreaNameOpt "" ""

parseAreaOptions :: [KenAllCsvRow] -> [KenAllCsvRow]
parseAreaOptions = map parseAreaOption

parseAreaOption :: KenAllCsvRow -> KenAllCsvRow
parseAreaOption r@(KenAllCsvRow {..}) = r{_areaName = parsedOpt}
  where 
    parsedOpt :: ParsedAreaName
    parsedOpt =
      case _areaName of
        (AreaNameOpt nm opt) ->
          case P.parse areaOptAddrs "" (T.pack opt) of
            Left err -> error $ show err -- TODO
            Right y -> (AreaNameParsedOpt nm y)
        _ -> _areaName

areaOptAddrs :: AreaOptionParser [AreaOptAddr]
areaOptAddrs =  areaOptAddr `P.sepBy` (P.char '、')

areaOptAddr :: AreaOptionParser AreaOptAddr
areaOptAddr = _areaOptAddrWithBranch <#> _areaOptAddr
  where
    _areaOptAddrWithBranch :: AreaOptionParser AreaOptAddr
    _areaOptAddrWithBranch =
      (AreaOptAddrWithBranch <$> (areaRange <* P.char '－') <*> areaRange)
      <#> (AreaOptAddrWithBranch <$> (areaRange <* P.char 'の') <*> areaRange)
    
    _areaOptAddr :: AreaOptionParser AreaOptAddr
    _areaOptAddr = AreaOptAddr <$> areaRange

areaNumNm :: AreaOptionParser AreaNumNm
areaNumNm = AreaNumNm <$> areaPrefix <*> areaNum <*> areaUnit <*> areaNotes

areaRange :: AreaOptionParser AreaRange
areaRange = _areaRange <#> _areaRangeNumOnly
  where
    _areaRangeNumOnly :: AreaOptionParser AreaRange
    _areaRangeNumOnly = AreaRangeNumOnly <$> areaNumNm
    
    _areaRange :: AreaOptionParser AreaRange
    _areaRange = AreaRange <$> (areaNumNm <* P.char '～') <*> areaNumNm

areaPrefix :: AreaOptionParser String
areaPrefix =
  P.many $
    P.noneOf $
      ['０'..'９'] ++ (concat [[l, r] | (l, r) <- noteBrackets])

areaNum :: AreaOptionParser AreaNum
areaNum = _areaNum <#> (return NoNum)
  where
    _areaNum :: AreaOptionParser AreaNum
    _areaNum = do
      txt <- P.many1 $ P.oneOf ['０'..'９']
      return $ AreaNum $ fullWidthNumToInt txt

units :: [String]
units =  ["丁目", "番地", "番", "林班", "地割", "区"]

areaUnit :: AreaOptionParser String
areaUnit = (P.choice [P.string x | x <- units]) <#> P.string ""

areaNotes :: AreaOptionParser [AreaNote]
areaNotes = P.many areaNote

noteBrackets :: [(Char, Char)]
noteBrackets = [('「', '」'), ('〔', '〕')]

areaNote :: AreaOptionParser AreaNote
areaNote =
  AreaParsedNote <$>
    (P.choice $ genNoteParsers noteBrackets)

excTerm :: P.Parser String
excTerm = P.string "を除く" <#> P.string "以外"

sepStrs :: P.Parser String
sepStrs = P.choice $ concat [[P.string [x], P.string [y]] | (x, y) <- noteBrackets]

noteField :: P.Parser String
noteField = P.many $ P.noneOf $ concat [[x, y] | (x, y) <- noteBrackets]

mkNoteParser :: (Char, Char) -> P.Parser ParsedNote
mkNoteParser (lsep, rsep) =
  --[ ExceptionNote <$> ((lp *> noteField <* excTerm) <* rp)
  --, ExceptionNote <$> ((lp *> noteField <* rp) <* excTerm)
  --, ParsedNote <$> (lp *> noteField <* rp) 
  --]
  (ExceptionNote  <$> ((P.between lp rp noteField) <* excTerm))
  <#> (ParsedNote <$> (P.between lp rp noteField))
  where
    lp = P.char lsep
    rp = P.char rsep

genNoteParsers :: [(Char, Char)] -> [AreaOptionParser ParsedNote]
genNoteParsers seps = map mkNoteParser seps

excludeSym :: AreaOptionParser String
excludeSym = P.string "を除く"

{-.オプションを単一化する -}

-- | 住所単位を振り分ける
normalizeAreaUnits :: [KenAllCsvRow] -> [KenAllCsvRow]
normalizeAreaUnits = map normalizeAreaUnit

normalizeAreaUnit :: KenAllCsvRow -> KenAllCsvRow
normalizeAreaUnit r = r{_areaName=aos}
  where
    an :: ParsedAreaName
    an = _areaName r
    aos =
      case an of 
        (AreaNameParsedOpt nm os) -> AreaNameParsedOpt nm $ normalizeAreaUnitAux os
        _ -> an

normalizeAreaUnitAux :: [AreaOptAddr] -> [AreaOptAddr]
normalizeAreaUnitAux [] = []
normalizeAreaUnitAux addrs = map (modifyUnit lastAreaUnit) addrs
  where
    lastAddr = last addrs
    lastRange =
      case lastAddr of
        (AreaOptAddr range) -> range
        (AreaOptAddrWithBranch _ right) -> right
    lastAreaNumNm =
      case lastRange of
        (AreaRangeNumOnly x) -> x
        (AreaRange _ x) -> x
    lastAreaUnit =
      case lastAreaNumNm of
        (AreaNumNm _ _ u _) -> u
        _ -> ""
    
    modifyUnit :: String -> AreaOptAddr -> AreaOptAddr
    modifyUnit u (AreaOptAddr range) = (AreaOptAddr (modifyUnit' u range))
    modifyUnit u a@(AreaOptAddrWithBranch {..}) = a{_right=modifyUnit' u _right}
    
    modifyUnit' :: String -> AreaRange -> AreaRange
    modifyUnit' u (AreaRangeNumOnly numnm) = AreaRangeNumOnly $ modifyUnit'' u numnm
    modifyUnit' u r@(AreaRange {..}) = r{_from=modifyUnit'' u _from, _to=modifyUnit'' u _to}
    
    modifyUnit'' :: String -> AreaNumNm -> AreaNumNm
    modifyUnit'' _ NoareaNumNm = NoareaNumNm
    modifyUnit'' u ann@(AreaNumNm {..}) = ann{_unit=u}

normalizeLeftAreaRange :: KenAllCsvRow -> KenAllCsvRow
normalizeLeftAreaRange =
  normalizeAreaRange _left (\_ r rngs -> [AreaOptAddr r' | r' <- init rngs] ++ [AreaOptAddrWithBranch (last rngs) r])

normalizeRightAreaRange :: KenAllCsvRow -> KenAllCsvRow
normalizeRightAreaRange =
  normalizeAreaRange _right (\l _ rngs -> [AreaOptAddrWithBranch l r' | r' <- rngs])

normalizeAreaRange :: (AreaOptAddr -> AreaRange) -> (AreaRange -> AreaRange -> [AreaRange] -> [AreaOptAddr]) -> KenAllCsvRow -> KenAllCsvRow
normalizeAreaRange accessor mergeFunc r =
  case (_areaName r) of 
    (AreaNameParsedOpt nm os) ->
      r{_areaName=AreaNameParsedOpt nm $ normalizeAreaRangeAux accessor mergeFunc os}
    _ -> r

normalizeAreaRangeAux :: (AreaOptAddr -> AreaRange) -> (AreaRange -> AreaRange -> [AreaRange] -> [AreaOptAddr]) -> [AreaOptAddr] -> [AreaOptAddr]
normalizeAreaRangeAux _ _ [] = []
normalizeAreaRangeAux bnchAccessor mergeFunc addrs = concatMap normalizeAreaRangeAux' addrs
  where
    normalizeAreaRangeAux' :: AreaOptAddr -> [AreaOptAddr]
    normalizeAreaRangeAux' (AreaOptAddr r) =
      [AreaOptAddr r' | r' <- normalizeAreaRangeAux'' r]
    normalizeAreaRangeAux' a@(AreaOptAddrWithBranch {..}) = mergeFunc _left _right $ normalizeAreaRangeAux'' $ bnchAccessor a
    
    normalizeAreaRangeAux'' :: AreaRange -> [AreaRange]
    normalizeAreaRangeAux'' l@(AreaRangeNumOnly _) = [l]
    normalizeAreaRangeAux'' (AreaRange f t) =
      [AreaRangeNumOnly areaNm | areaNm <- normalizeAreaRangeAux''' f t]
    
    normalizeAreaRangeAux''' :: AreaNumNm -> AreaNumNm -> [AreaNumNm]
    normalizeAreaRangeAux''' NoareaNumNm to = [to]
    normalizeAreaRangeAux''' from NoareaNumNm = [from]
    normalizeAreaRangeAux''' from to =
      [from{_num=(AreaNum i)} | i <- rng (_num from) (_num to)]
      where
        rng :: AreaNum -> AreaNum -> [Int]
        rng NoNum NoNum = []
        rng (AreaNum fi) NoNum = [fi]
        rng NoNum (AreaNum ti) = [ti]
        rng (AreaNum fi) (AreaNum ti) = [fi..ti]

normalizeAreaOptAddrs :: [KenAllCsvRow] -> [KenAllCsvRow]
normalizeAreaOptAddrs = concatMap normalizeAreaOptAddr

normalizeAreaOptAddr :: KenAllCsvRow -> [KenAllCsvRow]
normalizeAreaOptAddr r =
  case (_areaName r) of 
    (AreaNameParsedOpt nm os) -> [r{_areaName=AreaNameParsedOpt nm [o]} | o <- os]
    _ -> [r]

normalizeAreaRanges :: [KenAllCsvRow] -> [KenAllCsvRow]
normalizeAreaRanges = map (normalizeRightAreaRange . normalizeLeftAreaRange)

normalizeAreaOpts :: [KenAllCsvRow] -> [KenAllCsvRow]
normalizeAreaOpts =
  normalizeAreaOptAddrs
  . normalizeAreaRanges
  . normalizeAreaUnits
  . parseAreaOptions

parsedAreaNameToAreaName :: ParsedAreaName -> AreaName
parsedAreaNameToAreaName (AreaNameNoOpt s) = s
parsedAreaNameToAreaName (AreaNameOpt s1 s2) = s1 ++ s2
parsedAreaNameToAreaName (AreaNameBeginOpt s1 s2) = s1 ++ s2
parsedAreaNameToAreaName (AreaNameEndOpt s) = s
parsedAreaNameToAreaName (AreaNameParsedOpt s os) =
  concat (s:(map areaOptAddrToText os))
  where
    areaOptAddrToText :: AreaOptAddr -> String
    areaOptAddrToText (AreaOptAddr r) = areaRangeToText r
    areaOptAddrToText (AreaOptAddrWithBranch{..}) =
      concat [areaRangeToText _left, "－", areaRangeToText _right]

    areaRangeToText :: AreaRange -> String
    areaRangeToText (AreaRangeNumOnly n) = areaNumNmToText n
    areaRangeToText (AreaRange{..}) = 
      concat [areaNumNmToText _from, "～", areaNumNmToText _to]

    areaNumNmToText :: AreaNumNm -> String
    areaNumNmToText NoareaNumNm = ""
    areaNumNmToText (AreaNumNm{..}) =
      concat [_prefix, areaNumToText _num, _unit]

    areaNumToText :: AreaNum -> String
    areaNumToText NoNum = ""
    areaNumToText (AreaNum n) = show n

convertToNormalizedCsvRow :: KenAllCsvRow -> NormalizedCsvRow
convertToNormalizedCsvRow (KenAllCsvRow{..}) =
  NormalizedCsvRow _stateName _cityName _postalCode (parsedAreaNameToAreaName _areaName) _latitude _longitude

normalizeKenAllCsv :: Csv -> [NormalizedCsvRow]
normalizeKenAllCsv =
  (map convertToNormalizedCsvRow)
  . normalizeAreaOpts
  . margeRows
  . reduceKenAllCsv


type AreaGeoMap = M.Map String [AreaGeoInfo]
type AreaGeoInfo = (AreaName, Latitude, Longitude)

csvToAreaGeoInfo :: Csv -> AreaGeoMap
csvToAreaGeoInfo cs = M.fromListWith (++) [(stateNm r ++ cityNm r, [(areaNm r, lat r, lon r)]) | r <- cs]
  where
    stateNm :: CsvRow -> StateName
    stateNm r = r !! 0
    cityNm :: CsvRow -> CityName
    cityNm r = r !! 1
    areaNm :: CsvRow -> AreaName
    areaNm r = r !! 2
    lat :: CsvRow -> Latitude
    lat r = read (r !! 7) :: Latitude
    lon :: CsvRow -> Longitude
    lon r = read (r !! 8) :: Longitude

mixedCsvRowToText :: NormalizedCsvRow -> T.Text
mixedCsvRowToText (NormalizedCsvRow {..}) =
  T.intercalate (T.pack ",") 
    [
      T.pack _PostalCode
    , T.pack _StateName
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

hasLocation :: NormalizedCsvRow -> Bool
hasLocation (NormalizedCsvRow{..}) = _Latitude /= Nothing && _Longitude /= Nothing

mix :: AreaGeoMap -> [NormalizedCsvRow] -> [NormalizedCsvRow]
mix sgi = map (mixRow sgi)

mixRow :: AreaGeoMap -> NormalizedCsvRow -> NormalizedCsvRow
mixRow sgi row
  | hasLocation row = row
  | otherwise =
      case (lookupAreaGeoInfoList sgi (_StateName row) (_CityName row)) of
        Nothing -> row
        Just x -> 
          case findMinEditDistanceAreaName x (_AreaName row) of
            Nothing -> row
            Just (_, lat, lon) ->
              row{_Latitude = (Just lat), _Longitude = (Just lon)}

lookupAreaGeoInfoList :: AreaGeoMap -> StateName -> CityName -> Maybe([AreaGeoInfo])
lookupAreaGeoInfoList t sn cn = M.lookup (sn ++ cn) t


findMinEditDistanceAreaName :: [AreaGeoInfo] -> AreaName -> Maybe(AreaGeoInfo)
findMinEditDistanceAreaName [] _ = Nothing
findMinEditDistanceAreaName ax n =
  Just $ fst $ L.minimumBy cmp [(a, editDist an n) | a@(an, _, _) <- ax]
  where
    cmp :: (a, Int) -> (b, Int) -> Ordering
    cmp (_, c1) (_, c2)
      | c1 < c2 = LT
      | otherwise = GT

editDist :: String -> String -> Int
editDist = ED.levenshteinDistance ED.defaultEditCosts 

