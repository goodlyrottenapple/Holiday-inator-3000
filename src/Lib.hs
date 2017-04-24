{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE PolyKinds   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

-- module Lib
--     ( startApp
--     , app
--     ) where

module Lib where

import Protolude
import qualified Data.String as S
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B

import Data.Time
import Data.Time.Format (parseTimeM,formatTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.String.Conversions (cs)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import qualified Control.Monad.Parallel as P


data Date' a = Concrete a | Anytime deriving (Eq, Show)

instance Functor Date' where
    fmap _ Anytime = Anytime
    fmap f (Concrete a) = Concrete (f a)

-- instance Applicative Date' where
--     pure a = Concrete a

--     _ <*> Anytime = Anytime
--     Anytime <*> _ = Anytime
--     (Concrete f) <*> (Concrete a) = Concrete (f a)


type Date = Date' Day

parseDate :: Alternative a => Text -> a Date
parseDate s = if s == "anytime" 
        then pure Anytime 
        else case parseTimeM True defaultTimeLocale "%FT%X" (cs s) :: Maybe Day of
            Just d -> pure $ Concrete d
            Nothing -> empty

dateToText :: Date -> Text
dateToText Anytime = "anytime"
dateToText (Concrete d) = cs $ formatTime defaultTimeLocale "%F" d

instance ToHttpApiData Date where
    toUrlPiece = dateToText

instance FromJSON Date where
    parseJSON (String s) = parseDate s
    parseJSON _ = mzero

instance ToJSON Date where
    toJSON Anytime = String "anytime"
    toJSON (Concrete d) = String $ cs $ formatTime defaultTimeLocale "%FT00:00:00" d


newtype DateTime = DateTime UTCTime deriving (Eq, Show)

instance FromJSON DateTime where
    parseJSON (String s) = case parseTimeM True defaultTimeLocale "%FT%X" (cs s) :: Maybe UTCTime of
        Just d -> pure $ DateTime d
        Nothing -> mzero
    parseJSON _ = mzero 

instance ToJSON DateTime where
    toJSON (DateTime d) = String $ cs $ formatTime defaultTimeLocale "%FT%X" d

data Flight = Flight {
    _CarrierIds :: [Int]
  , _OriginId :: Int
  , _DestinationId :: Int
  , _DepartureDate :: Date
} deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Flight)


data Quote = Quote {
    _QuoteId :: Int
  , _MinPrice :: Int 
  , _Direct :: Bool
  , _OutboundLeg :: Flight
  , _InboundLeg :: Flight
  , _QuoteDateTime :: DateTime
} deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Quote)


data Place = Place {
    _PlaceId :: Int
  , _Name :: Text
  , _Type :: Text
  , _SkyscannerCode :: Text
} deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Place)


data Carrier = Carrier {
    _CarrierId :: Int
  , _Name :: Text
} deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Carrier)


data Currency = Currency {
    _Code :: Text
  , _Symbol :: Text
} deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Currency)


data BrowseQuotes = BrowseQuotes {
    _Quotes :: [Quote] 
  , _Places :: [Place]
  , _Carriers :: [Carrier]
  -- , _Currencies :: [Currency]
} deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BrowseQuotes)

-- quoteMap :: [Quote] -> IntMap Quote
-- quoteMap quotes = IM.fromList [(_QuoteId q , q) | q <- quotes]

type Places = IntMap Place
type Carriers = IntMap Carrier

mkIntMap :: (a -> Int) -> [a] -> IntMap a
mkIntMap f xs = IM.fromList [(f x , x) | x <- xs]

placeMap :: [Place] -> Places
placeMap = mkIntMap _PlaceId

carrierMap :: [Carrier] -> Carriers
carrierMap = mkIntMap _CarrierId


ppFlight :: Places -> Carriers -> Flight -> Text
ppFlight pM cM (Flight cIds o d t) = 
    "\n      ===========================" <>
    "\n      Carriers: " <> carriersStr <>
    "\n      Origin: " <> origin <>
    "\n      Destination: " <> destination <>
    "\n      Departure Date: " <> show t <>
    "\n      ==========================="
    where
        carriers = map (\id -> (_Name :: Carrier -> Text) $ IM.findWithDefault (Carrier 0 "Unknown") id cM) cIds
        carriersStr = foldr (<>) "" (intersperse " " carriers) :: Text
        origin = (_Name :: Place -> Text) $ IM.findWithDefault (Place 0 "Unknown" "" "") o pM
        destination = (_Name :: Place -> Text) $ IM.findWithDefault (Place 0 "Unknown" "" "") d pM

ppQuote :: Places -> Carriers -> Quote -> Text
ppQuote pM cM (Quote _ minPrice direct outboundLeg inboundLeg (DateTime d)) = 
    "\n=========================================" <>
    "\nPrice: " <> show minPrice <>
    "\nDirect: " <> show direct <>
    "\nOutbound Leg: " <> ppFlight pM cM outboundLeg <>
    "\nInbound Leg: " <> ppFlight pM cM inboundLeg <>
    "\nQuote from: " <> show d <>
    "\n========================================="

infixl 8 :>>
-- hides the fact that we need to add v1.0 to every query....
type (a :: k1) :>> (b :: k) = a :> "v1.0" :> b 

type ApiKey = Text

apiKey :: Maybe ApiKey
apiKey = Nothing

type SkyscannerAPI = 
        "browsequotes" :> "v1.0" :>
        "UK" :> -- Capture "country" Text :>
        "gbp" :> -- Capture "currency" Text :>
        "en-UK" :> -- Capture "locale" Text :>
        Capture "originPlace" Text :>
        Capture "destinationPlace" Text :>
        Capture "outboundPartialDate" Date :>
        Capture "inboundPartialDate" Date :>
        QueryParam "apiKey" ApiKey :> Get '[JSON] BrowseQuotes 
    -- :<|> 
    --     "autosuggest" :> "v1.0" :> "UK" :> "gbp" :> "en-UK" :> 
    --     QueryParam "id" Int :> 
    --     QueryParam "apiKey" ApiKey :> Get '[JSON] [Place]
    -- "reference" :>> "currencies" :> QueryParam "apiKey" ApiKey :> Get '[JSON] Value


api :: Proxy SkyscannerAPI
api = Proxy

browseQuotes :: Text -> Text -> Date -> Date -> Maybe ApiKey -> ClientM BrowseQuotes
-- getPlace :: Int -> Maybe ApiKey -> ClientM [Place]
browseQuotes = client api -- (BaseUrl Http "hackage.haskell.org" 80)


sampleQuery :: Date ->  Date -> ClientM BrowseQuotes
sampleQuery outDate inDate = browseQuotes "LOND-sky" "anywhere" outDate inDate apiKey


today :: IO Date
today = do { t <- getCurrentTime; return $ Concrete $ utctDay t}


-- hack for the ghci....
instance S.IsString Date where
    fromString s = case parseTimeM True defaultTimeLocale "%F" s :: Maybe Day of
        Just day -> Concrete day
        Nothing -> Anytime


data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show,Eq,Enum,Ord)


-- takes current week day and desired one and calculates the difference
weekDayDiff :: WeekDay -> WeekDay -> Int
weekDayDiff current wanted | cInt <- fromEnum current , wInt <- fromEnum wanted = 
    if cInt <= wInt then wInt - cInt else 7 - (cInt - wInt) 

getWeekDay :: Day -> WeekDay
getWeekDay d | (_,_,r) <- toWeekDate d = toEnum (r-1)

getClosestWeekDay :: WeekDay -> Day -> Day
getClosestWeekDay wkDay tod = addDays (toInteger $ weekDayDiff (getWeekDay tod) wkDay) tod

genNInInterval :: Functor f => Int -> Int -> f Day -> [(f Day, f Day)]
genNInInterval _ 0 _ = []
genNInInterval offset n day = (day, map (addDays $ toInteger offset) day) : genNInInterval offset (n-1) (map (addDays 7) day) 

possibleWeekends :: [(WeekDay,WeekDay)]
possibleWeekends = [(Thu,Sun) , (Wed,Sun) , (Thu,Mon) , (Fri,Mon) , (Fri,Tue), (Fri,Sun) , (Sat,Mon)]

genNWeekends :: Functor f => Int -> f Day -> [(f Day, f Day)]
genNWeekends n d = concat $ map (\(a,b) -> genNInInterval (weekDayDiff a b) n (map (getClosestWeekDay a) d)) possibleWeekends

cartProd :: [a] -> [b] -> [(a, b)]
cartProd = liftM2 (,)


runBrowseQuotes :: Text -> Date -> Date -> IO (Either ServantError BrowseQuotes)
runBrowseQuotes origin outD inD = do
    manager <- newManager defaultManagerSettings
    runClientM (browseQuotes origin "anywhere" outD inD apiKey) (ClientEnv manager (BaseUrl Http "partners.api.skyscanner.net" 80 "/apiservices"))



runBrowseQuotesVerbose :: Text -> Date -> Date -> IO BrowseQuotes
runBrowseQuotesVerbose origin outD inD = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (browseQuotes origin "anywhere" outD inD apiKey) (ClientEnv manager (BaseUrl Http "partners.api.skyscanner.net" 80 "/apiservices"))
    case res of
        Left err -> do
            putStrLn $ "Error: " ++ show err
            return $ BrowseQuotes [] [] []
        Right r -> do
            print ( "Returned for: " <> show origin <> " - " <> show outD <> " - " <> show inD :: Text)
            return r
            

-- run :: Text -> Date -> Date -> IO ()
-- run origin outD inD = do
--     manager <- newManager defaultManagerSettings
--     res <- runClientM (browseQuotes origin "anywhere" outD inD apiKey) (ClientEnv manager (BaseUrl Http "partners.api.skyscanner.net" 80 "/apiservices"))
--     case res of
--         Left err -> putStrLn $ "Error: " ++ show err
--         Right r -> 
--             let pM = placeMap $ _Places r
--                 cM = carrierMap $ _Carriers r in
--             putStrLn $ foldr (<>) "" $ map (ppQuote pM cM) $ filter (\q -> _MinPrice q < 55) $ _Quotes r

-- foldIO :: Foldable t => (a -> IO ()) -> t a -> IO ()
-- foldIO f t = foldM (\() a -> f a) () t

-- runAll :: Int -> Date -> IO ()
-- runAll noOfWeeks tod = foldIO (\(o,(a,b)) -> run o a b) (["EMA-sky", "BHX-sky", "LOND-sky"] `cartProd` (genNWeekends noOfWeeks tod))

instance Semigroup BrowseQuotes where
    (BrowseQuotes a b c) <> (BrowseQuotes a' b' c') = BrowseQuotes (a ++ a') (b ++ b') (c ++ c')

instance Monoid BrowseQuotes where
    mempty = BrowseQuotes [] [] []
    mappend = (<>)

runAll :: Int -> Date -> IO BrowseQuotes
runAll nOfWeeks tod = foldM aux mempty (["EMA-sky", "BHX-sky", "LTN-sky" , "STN-sky"] `cartProd` (genNWeekends nOfWeeks tod))
    where 
        aux bq (o,(a,b)) = do
            bq' <- runBrowseQuotesVerbose o a b
            return $ bq <> bq'

runParallel :: Int -> Date -> IO [Either ServantError BrowseQuotes]
runParallel nOfWeeks tod = P.sequence $ map (\(o,(a,b)) -> runBrowseQuotes o a b) 
    (["EMA-sky", "BHX-sky", "LTN-sky" , "STN-sky"] `cartProd` (genNWeekends nOfWeeks tod))

runParallelIsh :: Int -> Date -> IO [Either ServantError BrowseQuotes]
runParallelIsh nOfWeeks tod = do
    res <- sequence $ map (\o -> P.sequence $ map (\(a,b) -> runBrowseQuotes o a b) (genNWeekends nOfWeeks tod)) 
        ["EMA-sky", "BHX-sky", "LTN-sky" , "STN-sky"]
    return $ concat res

run :: Int -> Date -> IO ()
run nOfWeeks tod = do
    res <- runAll nOfWeeks tod
    B.writeFile "./out.json" $ encodePretty res


runP :: Int -> Date -> IO ()
runP nOfWeeks tod = do
    res <- runParallelIsh nOfWeeks tod
    printErrors res
    B.writeFile "./out.json" $ encodePretty (foldr (<>) mempty (rights res))
    where
        printErrors :: [Either ServantError a] -> IO ()
        printErrors [] = return ()
        printErrors ((Left err):xs) = do
            putStrLn $ "Error: " ++ show err
            printErrors xs
        printErrors (_:xs) = printErrors xs

loadDB :: FilePath -> IO BrowseQuotes
loadDB fp = do
    f <- B.readFile fp
    case decode f :: Maybe BrowseQuotes of
        Just res -> return res
        Nothing -> return mempty

queryDB :: BrowseQuotes -> Int -> IO ()
queryDB db maxPrice =
    let pM = placeMap $ _Places db
        cM = carrierMap $ _Carriers db in
    putStrLn $ foldr (<>) "" $ map (ppQuote pM cM) $ filter (\q -> _MinPrice q <= maxPrice) $ _Quotes db


