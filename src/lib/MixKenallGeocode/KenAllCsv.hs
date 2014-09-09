-- vim: set ts=2 sw=2 sts=0 ff=unix foldmethod=indent:

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MixKenallGeocode.KenAllCsv
--   (
--     SimpleKenAllCsvRow(..)
--   , KenAllCsvRow(..)
--   , ParsedAreaName(..)
--   , AreaOptAddr(..)
--   , AreaRange(..)
--   , AreaNumNm(..)
--   , AreaNum(..)
--   , AreaNote(..)
--   , ParsedNote(..)
--   , normalizeKenAllCsv
--   )
where

import MixKenallGeocode.Csv
import MixKenallGeocode.CommonType
import Control.Applicative
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Text as P

data SimpleKenAllCsvRow = SimpleKenAllCsvRow
  { _pCode :: PostalCode
  , _sName :: StateName
  , _cName :: CityName
  , _aName :: AreaName
  }
  deriving (Show, Eq)

data KenAllCsvRow = KenAllCsvRow
  { _postalCode :: PostalCode
  , _stateName :: StateName
  , _cityName :: CityName
  , _areaName :: ParsedAreaName
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

-- 毎回 try するのが面倒なので、演算子を定義
(<#>) :: (P.ParsecT s u m a) -> (P.ParsecT s u m a) -> (P.ParsecT s u m a)
(<#>) a b = P.try a <|> b
infixl 3 <#>

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
  { _postalCode = r !! 2
  , _stateName =  r !! 6
  , _cityName = r !! 7
  , _areaName = parsedArNm
  }
  where
    arNm = r!!8
    parsedArNm =
      case P.parse parsedAreaName "* Parse Error *" (T.pack arNm) of
        Left err -> error $ show err
        Right x -> x

{- 複数行に渡る行をマージ -}

margeRows :: [KenAllCsvRow] -> [KenAllCsvRow]
margeRows [] = []
margeRows (r:rs) =
  case (_areaName r) of
    AreaNameBeginOpt x y ->
      let (mr, rest) = splitMargedRowAndRest r rs
      in mr:(margeRows rest)
    otherwise -> r:(margeRows rs)

splitMargedRowAndRest :: KenAllCsvRow -> [KenAllCsvRow] -> (KenAllCsvRow, [KenAllCsvRow])
splitMargedRowAndRest r rs = (r{_areaName = marged_r}, tail rest)
  where
    (xs, rest) = break hasAreaNameEndOpt rs
    marged_r = margeParsedAreaNames (_areaName r) $ map _areaName $ xs ++ [head rest]
    hasAreaNameEndOpt :: KenAllCsvRow -> Bool
    hasAreaNameEndOpt r@(KenAllCsvRow {..}) =
      case _areaName of
        (AreaNameEndOpt _) -> True
        otherwise -> False

margeParsedAreaNames :: ParsedAreaName -> [ParsedAreaName] -> ParsedAreaName
margeParsedAreaNames a@(AreaNameBeginOpt an opt) as = AreaNameOpt an $ concat $ opt:(map takeOptStr as)
  where
    takeOptStr :: ParsedAreaName -> String
    takeOptStr r@(AreaNameNoOpt x) = x
    takeOptStr r@(AreaNameEndOpt x) = x


parseAreaOptions :: [KenAllCsvRow] -> [KenAllCsvRow]
parseAreaOptions = map parseAreaOption

parseAreaOption :: KenAllCsvRow -> KenAllCsvRow
parseAreaOption r@(KenAllCsvRow {..}) = r{_areaName = parsedOpt}
  where 
    parsedOpt :: ParsedAreaName
    parsedOpt =
      case _areaName of
        x@(AreaNameOpt nm opt) ->
          case P.parse areaOptAddrs "" (T.pack opt) of
            Left err -> error $ show err -- TODO
            Right y -> (AreaNameParsedOpt nm y)
        otherwise -> _areaName

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
-- noteField = P.manyTill P.anyChar (sepStrs <#> excTerm)
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
        (AreaNameParsedOpt nm opts) -> AreaNameParsedOpt nm $ normalizeAreaUnitAux opts
        otherwise -> an

normalizeAreaUnitAux :: [AreaOptAddr] -> [AreaOptAddr]
normalizeAreaUnitAux [] = []
normalizeAreaUnitAux addrs = map (modifyUnit lastAreaUnit) addrs
  where
    lastAddr = last addrs
    lastRange =
      case lastAddr of
        (AreaOptAddr range) -> range
        (AreaOptAddrWithBranch left right) -> right
    lastAreaNumNm =
      case lastRange of
        (AreaRangeNumOnly x) -> x
        (AreaRange _ x) -> x
    lastAreaUnit =
      case lastAreaNumNm of
        (AreaNumNm _ _ u _) -> u
        otherwise -> ""
    
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
  normalizeAreaRange _left (\_ r rngs -> [AreaOptAddr r | r <- init rngs] ++ [AreaOptAddrWithBranch (last rngs) r])

normalizeRightAreaRange :: KenAllCsvRow -> KenAllCsvRow
normalizeRightAreaRange =
  normalizeAreaRange _right (\l r rngs -> [AreaOptAddrWithBranch l r' | r' <- rngs])

normalizeAreaRange :: (AreaOptAddr -> AreaRange) -> (AreaRange -> AreaRange -> [AreaRange] -> [AreaOptAddr]) -> KenAllCsvRow -> KenAllCsvRow
normalizeAreaRange accessor mergeFunc r =
  case (_areaName r) of 
    (AreaNameParsedOpt nm opts) ->
      r{_areaName=AreaNameParsedOpt nm $ normalizeAreaRangeAux accessor mergeFunc opts}
    otherwise -> r

normalizeAreaRangeAux :: (AreaOptAddr -> AreaRange) -> (AreaRange -> AreaRange -> [AreaRange] -> [AreaOptAddr]) -> [AreaOptAddr] -> [AreaOptAddr]
normalizeAreaRangeAux _ _ [] = []
normalizeAreaRangeAux bnchAccessor mergeFunc addrs = concatMap normalizeAreaRangeAux' addrs
  where
    normalizeAreaRangeAux' :: AreaOptAddr -> [AreaOptAddr]
    normalizeAreaRangeAux' a@(AreaOptAddr r) =
      [AreaOptAddr r' | r' <- normalizeAreaRangeAux'' r]
    normalizeAreaRangeAux' a@(AreaOptAddrWithBranch {..}) = mergeFunc _left _right $ normalizeAreaRangeAux'' $ bnchAccessor a
    
    normalizeAreaRangeAux'' :: AreaRange -> [AreaRange]
    normalizeAreaRangeAux'' l@(AreaRangeNumOnly _) = [l]
    normalizeAreaRangeAux'' l@(AreaRange f t) =
      [AreaRangeNumOnly areaNm | areaNm <- normalizeAreaRangeAux''' f t]
    
    normalizeAreaRangeAux''' :: AreaNumNm -> AreaNumNm -> [AreaNumNm]
    normalizeAreaRangeAux''' NoareaNumNm to = [to]
    normalizeAreaRangeAux''' from NoareaNumNm = [from]
    normalizeAreaRangeAux''' from to =
      [from{_num=(AreaNum i)} | i <- rng (_num from) (_num to)]
      where
        rng :: AreaNum -> AreaNum -> [Int]
        rng (AreaNum fi) (AreaNum ti) = [fi..ti]
        rng (AreaNum fi) NoNum = [fi]
        rng NoNum (AreaNum ti) = [ti]

normalizeAreaOptAddrs :: [KenAllCsvRow] -> [KenAllCsvRow]
normalizeAreaOptAddrs = concatMap normalizeAreaOptAddr

normalizeAreaOptAddr :: KenAllCsvRow -> [KenAllCsvRow]
normalizeAreaOptAddr r =
  case (_areaName r) of 
    (AreaNameParsedOpt nm opts) -> [r{_areaName=AreaNameParsedOpt nm [o]} | o <- opts]
    otherwise -> [r]

normalizeAreaRanges :: [KenAllCsvRow] -> [KenAllCsvRow]
normalizeAreaRanges = map (normalizeRightAreaRange . normalizeLeftAreaRange)

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
parsedAreaNameToAreaName (AreaNameParsedOpt s opts) =
  concat (s:(map areaOptAddrToText opts))
  where
    areaOptAddrToText :: AreaOptAddr -> String
    areaOptAddrToText (AreaOptAddr r) = areaRangeToText r
    areaOptAddrToText a@(AreaOptAddrWithBranch{..}) =
      concat [areaRangeToText _left, "－", areaRangeToText _right]

    areaRangeToText :: AreaRange -> String
    areaRangeToText (AreaRangeNumOnly n) = areaNumNmToText n
    areaRangeToText r@(AreaRange{..}) = 
      concat [areaNumNmToText _from, "～", areaNumNmToText _to]

    areaNumNmToText :: AreaNumNm -> String
    areaNumNmToText NoareaNumNm = ""
    areaNumNmToText (AreaNumNm{..}) =
      concat [_prefix, areaNumToText _num, _unit]

    areaNumToText :: AreaNum -> String
    areaNumToText NoNum = ""
    areaNumToText (AreaNum n) = show n

normalizeKenAllCsv :: Csv -> [SimpleKenAllCsvRow]
normalizeKenAllCsv =
  (map (\r -> SimpleKenAllCsvRow (_postalCode r) (_stateName r) (_cityName r) (parsedAreaNameToAreaName ((_areaName r)))))
  . normalizeAreaOpts
  . margeRows
  . reduceKenAllCsv
