import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when)

-- Define the Person data type
data Person = Person {
    name :: String,
    phoneNumber :: String,
    place :: String,
    email :: String
} deriving Show

-- Define CarID type and Cars map type (Map of CarID to availability)
type CarID = Int
type Cars = Map CarID Bool -- True = available, False = rented
type Rentals = Map CarID Person -- Map of CarID to the Person renting the car

-- Initial state with 5 cars (all available)
initialCars :: Cars
initialCars = Map.fromList [(1, True), (2, True), (3, True), (4, True), (5, True)]

-- Initial state with no rentals
initialRentals :: Rentals
initialRentals = Map.empty

-- Rent a car: takes a car ID, cars, and rentals, and returns updated cars and rentals
rentCar :: CarID -> Person -> Cars -> Rentals -> (Cars, Rentals, String)
rentCar carId person cars rentals =
    case Map.lookup carId cars of
        Just True  -> 
            let newCars = Map.insert carId False cars  -- Mark car as rented
                newRentals = Map.insert carId person rentals  -- Add to rentals map
            in (newCars, newRentals, "Car " ++ show carId ++ " rented successfully to " ++ name person ++ "!")
        Just False -> (cars, rentals, "Car " ++ show carId ++ " is already rented.")
        Nothing    -> (cars, rentals, "Car " ++ show carId ++ " does not exist.")

-- Return a car: takes a car ID, cars, and rentals, and returns updated cars and rentals
returnCar :: CarID -> Cars -> Rentals -> (Cars, Rentals, String)
returnCar carId cars rentals =
    case Map.lookup carId rentals of
        Just _ -> 
            let newCars = Map.insert carId True cars  -- Mark car as available
                newRentals = Map.delete carId rentals  -- Remove from rentals map
            in (newCars, newRentals, "Car " ++ show carId ++ " returned successfully!")
        Nothing -> (cars, rentals, "Car " ++ show carId ++ " was not rented.")
        
-- List available cars
availableCars :: Cars -> [CarID]
availableCars cars = Map.keys $ Map.filter (== True) cars

-- Main interactive loop
main :: IO ()
main = do
    putStrLn "Welcome to the Car Rental System!"
    putStrLn "Please enter your details:"
    person <- getPersonDetails
    interactiveLoop initialCars initialRentals person

-- Function to get Person details from user input
getPersonDetails :: IO Person
getPersonDetails = do
    putStr "Enter your name: "
    name <- getLine
    putStr "Enter your phone number: "
    phoneNumber <- getLine
    putStr "Enter your place: "
    place <- getLine
    putStr "Enter your email: "
    email <- getLine
    return $ Person { name = name, phoneNumber = phoneNumber, place = place, email = email }

-- Main interactive loop to handle user input
interactiveLoop :: Cars -> Rentals -> Person -> IO ()
interactiveLoop cars rentals person = do
    putStrLn "\nChoose an option:"
    putStrLn "1. Rent a car"
    putStrLn "2. Return a car"
    putStrLn "3. Check available cars"
    putStrLn "4. Exit"
    putStr "Enter your choice: "
    choice <- getLine

    case choice of
        "1" -> do
            putStr "Enter the Car ID to rent: "
            carId <- getCarID
            let (newCars, newRentals, msg) = rentCar carId person cars rentals
            putStrLn msg
            interactiveLoop newCars newRentals person

        "2" -> do
            putStr "Enter the Car ID to return: "
            carId <- getCarID
            let (newCars, newRentals, msg) = returnCar carId cars rentals
            putStrLn msg
            interactiveLoop newCars newRentals person

        "3" -> do
            putStrLn $ "Available cars: " ++ show (availableCars cars)
            interactiveLoop cars rentals person

        "4" -> putStrLn "Thank you for using the Car Rental System!"

        _ -> do
            putStrLn "Invalid option. Please try again."
            interactiveLoop cars rentals person

-- Helper function to safely read CarID
getCarID :: IO CarID
getCarID = do
    input <- getLine
    case reads input of
        [(carId, "")] -> return carId
        _             -> do
            putStrLn "Invalid Car ID. Please enter a valid number:"
            getCarID