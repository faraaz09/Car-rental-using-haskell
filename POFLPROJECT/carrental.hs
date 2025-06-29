{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Web.Scotty as Scotty

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Time (Day, UTCTime, addDays, getCurrentTime, utctDay)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import Web.Scotty (ActionM, ScottyM, json, jsonData, param, post, get, middleware)

-- Database connection
getConnection :: IO PG.Connection
getConnection = do
  dbHost <- fromMaybe "localhost" <$> lookupEnv "DB_HOST"
  dbPort <- fromMaybe "5432" <$> lookupEnv "DB_PORT"
  dbUser <- fromMaybe "postgres" <$> lookupEnv "DB_USER"
  dbPass <- fromMaybe "" <$> lookupEnv "DB_PASSWORD"
  dbName <- fromMaybe "car_rental" <$> lookupEnv "DB_NAME"
  
  PG.connect $ PG.defaultConnectInfo
    { PG.connectHost = dbHost
    , PG.connectPort = read dbPort
    , PG.connectUser = dbUser
    , PG.connectPassword = dbPass
    , PG.connectDatabase = dbName
    }

-- Data types
data Car = Car
  { carId :: Int
  , make :: T.Text
  , model :: T.Text
  , year :: Int
  , carType :: T.Text
  , seats :: Int
  , pricePerDay :: Int
  , imageUrl :: Maybe T.Text
  , features :: [T.Text]
  } deriving (Generic, Show)

instance A.ToJSON Car where
  toJSON car = A.object
    [ "id" .= carId car
    , "make" .= make car
    , "model" .= model car
    , "year" .= year car
    , "type" .= carType car
    , "seats" .= seats car
    , "pricePerDay" .= pricePerDay car
    , "image" .= imageUrl car
    , "features" .= features car
    ]

data ReservationRequest = ReservationRequest
  { rrCarId :: Int
  , rrPickupDate :: Day
  , rrReturnDate :: Day
  , rrCustomerName :: T.Text
  , rrCustomerEmail :: T.Text
  , rrCustomerPhone :: T.Text
  } deriving (Generic, Show)

instance A.FromJSON ReservationRequest where
  parseJSON = A.withObject "ReservationRequest" $ \v -> ReservationRequest
    <$> v .: "carId"
    <*> v .: "pickupDate"
    <*> v .: "returnDate"
    <*> v .: "customerName"
    <*> v .: "customerEmail"
    <*> v .: "customerPhone"

data ReservationResponse = ReservationResponse
  { resSuccess :: Bool
  , resMessage :: Maybe T.Text
  , resConfirmationNumber :: T.Text
  , resTotalPrice :: Int
  } deriving (Generic, Show)

instance A.ToJSON ReservationResponse where
  toJSON res = A.object
    [ "success" .= resSuccess res
    , "message" .= resMessage res
    , "confirmationNumber" .= resConfirmationNumber res
    , "totalPrice" .= resTotalPrice res
    ]

-- API endpoints
main :: IO ()
main = do
  conn <- getConnection
  Scotty.scotty 3000 $ do
    middleware $ Cors.simpleCors Cors.simpleCorsResourcePolicy
      { Cors.corsRequestHeaders = ["Content-Type"]
      }
    
    -- Cars API
    get "/api/cars" $ do
      cars <- liftIO $ getCars conn
      json cars
    
    -- Reservations API
    post "/api/reservations" $ do
      reservationReq <- jsonData
      now <- liftIO getCurrentTime
      let days = diffDays (rrReturnDate reservationReq) (rrPickupDate reservationReq)
      
      -- Validate dates
      when (days <= 0) $ Scotty.raiseStatus 400 "Return date must be after pickup date"
      
      -- Check car availability
      carAvailable <- liftIO $ checkCarAvailability conn (rrCarId reservationReq) 
                                                   (rrPickupDate reservationReq) 
                                                   (rrReturnDate reservationReq)
      when (not carAvailable) $ Scotty.raiseStatus 400 "Car not available for selected dates"
      
      -- Get car price
      carPrice <- liftIO $ getCarPrice conn (rrCarId reservationReq)
      let totalPrice = fromIntegral days * carPrice
      
      -- Create reservation
      reservationId <- liftIO nextRandom
      liftIO $ createReservation conn reservationId (rrCarId reservationReq)
        (rrCustomerName reservationReq) (rrCustomerEmail reservationReq)
        (rrCustomerPhone reservationReq) (rrPickupDate reservationReq)
        (rrReturnDate reservationReq) totalPrice
      
      -- Send confirmation email (would be implemented in production)
      liftIO $ sendConfirmationEmail (rrCustomerEmail reservationReq) (show reservationId)
      
      json $ ReservationResponse True Nothing (show reservationId) totalPrice

-- Database operations
getCars :: PG.Connection -> IO [Car]
getCars conn = do
  -- In a real app, we'd query the database
  -- For demo, we'll return some sample data
  return
    [ Car 1 "Toyota" "Camry" 2022 "sedan" 5 45 
      (Just "https://images.unsplash.com/photo-1618843479313-40f8afb4b4d8?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80")
      ["Bluetooth", "Backup Camera", "Keyless Entry"]
    , Car 2 "Honda" "CR-V" 2021 "suv" 5 65 
      (Just "https://images.unsplash.com/photo-1568605117036-5fe5e7bab0b7?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80")
      ["Apple CarPlay", "AWD", "Sunroof"]
    , Car 3 "BMW" "X5" 2023 "luxury" 5 120 
      (Just "https://images.unsplash.com/photo-1555215695-3004980ad54e?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80")
      ["Leather Seats", "Navigation", "Premium Sound"]
    , Car 4 "Ford" "Mustang" 2022 "sports" 4 90 
      (Just "https://images.unsplash.com/photo-1593941707882-a5bba53b0998?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80")
      ["Convertible", "Sport Mode", "Premium Wheels"]
    , Car 5 "Chevrolet" "Tahoe" 2021 "suv" 8 85 
      (Just "https://images.unsplash.com/photo-1553440569-bcc63803a83d?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80")
      ["Third Row", "Towing Package", "DVD System"]
    , Car 6 "Tesla" "Model 3" 2023 "sedan" 5 75 
      (Just "https://images.unsplash.com/photo-1551836022-d5d88e9218df?ixlib=rb-1.2.1&auto=format&fit=crop&w=1350&q=80")
      ["Electric", "Autopilot", "Touchscreen"]
    ]

getCarPrice :: PG.Connection -> Int -> IO Int
getCarPrice conn carId = do
  cars <- getCars conn
  case filter (\c -> carId c == carId) cars of
    [car] -> return $ pricePerDay car
    _ -> return 50  -- Default price if car not found

checkCarAvailability :: PG.Connection -> Int -> Day -> Day -> IO Bool
checkCarAvailability conn carId pickupDate returnDate = do
  -- In a real app, we'd query the database for overlapping reservations
  -- For demo, we'll assume the car is available
  return True

createReservation :: PG.Connection -> UUID -> Int -> T.Text -> T.Text -> T.Text -> Day -> Day -> Int -> IO ()
createReservation conn reservationId carId customerName customerEmail customerPhone pickupDate returnDate totalPrice = do
  -- In a real app, we'd insert into database
  -- For demo, we'll just log it
  putStrLn $ "Created reservation: " ++ show reservationId
  putStrLn $ "Car ID: " ++ show carId
  putStrLn $ "Customer: " ++ T.unpack customerName
  putStrLn $ "Email: " ++ T.unpack customerEmail
  putStrLn $ "Phone: " ++ T.unpack customerPhone
  putStrLn $ "Pickup: " ++ show pickupDate
  putStrLn $ "Return: " ++ show returnDate
  putStrLn $ "Total: $" ++ show totalPrice

sendConfirmationEmail :: T.Text -> T.Text -> IO ()
sendConfirmationEmail email confirmationNumber = do
  -- In a real app, we'd send an actual email
  putStrLn $ "Sending confirmation email to: " ++ T.unpack email
  putStrLn $ "Confirmation number: " ++ T.unpack confirmationNumber

-- Helper functions
diffDays :: Day -> Day -> Int
diffDays d1 d2 = fromInteger $ toInteger $ floor (diffUTCTime (utcTime d1) (utcTime d2)) `div` (24 * 60 * 60)
  where
    utcTime d = UTCTime d 0