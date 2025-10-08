{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

module Main where

import Control.Exception (catch, handle, IOException)
import Control.Monad (unless, when)
import Control.Monad.State(execState, modify, State)
import Data.Char (toLower, toUpper)
import Data.Functor ((<&>))
import Data.List (find, intercalate, sortOn)
import Data.Maybe (fromMaybe)
import qualified Control.Monad.State as State
import qualified Data.Ord
import System.Console.ANSI
import System.Directory (renameFile)
import System.IO (hPutStrLn, IOMode (WriteMode), readFile, withFile, writeFile)
import Text.Read (readMaybe)

-- Data Types
-- Role Data Type
data Role = Community | Environment | Health | Wildlife deriving (Show, Read, Eq)

-- Availability Data Type
data Availability = Available | Unavailable deriving (Show, Read, Eq)

-- Volunteer Data Type
data Volunteer = Volunteer
    { volunteerID :: String
    , name :: String
    , contactInformation :: String
    , role :: Role
    , availability :: Availability
    , hoursContributed :: Double
    , noOfEventsParticipated :: Int
    } deriving (Show)

-- Menu Data Type
data Menu = MainMenu | EditMenu | SortMenu | OrderMenu | FilterMenu | RoleMenu | AvailabilityMenu | ReportMenu deriving (Show, Eq)

-- Validation Data Type
data Validation a = Success a | Failure String deriving (Show, Eq)

-- Operation Data Type
data Operation = Add | Delete | Search | Edit deriving (Show, Eq)

-- Report Data Type
data Report = Report
    { reportTitle :: String
    , reportContent :: [Volunteer] -> [String]}

-- State Monad for Performing Operations
type VolunteerState a = State [Volunteer] a

-- MenuOption Class 
class MenuOption a where
    getOptions :: a -> [(String, String)]
    validateOption :: a -> String -> Bool
    parseOption :: a -> String -> Maybe a

-- MenuOption Instance for Menu Data Type
instance MenuOption Menu where
    getOptions MainMenu = zip ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
        ["Add Volunteer", "Delete Volunteer", "Search Volunteer", "Edit Volunteer", "Display All Volunteers", "Sort Volunteer", "Filter Volunteer", "Report Summary", "Exit"]
    getOptions EditMenu = zip ["1", "2", "3", "4", "5", "6"]
        ["Name", "Contact Information", "Role", "Availability", "Hours contributed", "Number of Events Participated"]
    getOptions SortMenu = zip ["1", "2", "3", "4", "5"]
        ["Sort by Volunteer ID", "Sort by Name", "Sort by Contact Information", "Sort by Hours Contributed", "Sort by Number of Events participated"]
    getOptions OrderMenu = zip ["1", "2"]
        ["From Smallest to Largest", "From Largest to Smallest"]
    getOptions FilterMenu = zip ["1", "2"]
        ["Filter By Role", "Filter by Availability"]
    getOptions RoleMenu = zip ["1", "2", "3", "4"]
        ["Community", "Environment", "Health", "Wildlife"]
    getOptions AvailabilityMenu = zip ["1", "2"]
        ["Available", "Unavailable"]
    getOptions ReportMenu = zip ["1", "2", "3", "4", "5"]
        ["Volunteers by Role", "Volunteers by Availability", "Top Volunteers by Contributed Hours", "Volunteers Status", "Volunteers Participation Summary"]

    validateOption menu choice = choice `elem` map fst (getOptions menu)
    parseOption menu choice = if validateOption menu choice then Just menu else Nothing

-- Functor Instance for Validation
instance Functor Validation where
    fmap f (Success a) = Success (f a)
    fmap _ (Failure e) = Failure e

-- Applicative Instance for Validation
instance Applicative Validation where
    pure = Success
    Success f <*> Success a = Success (f a)
    Failure e <*> _= Failure e
    _ <*> Failure e = Failure e

-- Monad Instance for Validation
instance Monad Validation where 
    return = pure 
    Success a >>= f = f a
    Failure e >>= _ = Failure e

-- Semigroup Instance for Report
instance Semigroup Report where
    r1 <> r2 = Report
        {reportTitle = reportTitle r1 <> reportTitle r2,
        reportContent = \vs -> reportContent r1 vs <> reportContent r2 vs}

-- Monoid Instance for Report
instance Monoid Report where
    mempty = Report "" (const [])
    mappend = (<>)

-- Print ASCII Art Welcome Message
printAsciiArt :: IO ()
printAsciiArt = clearScreen
    >> setSGR [SetColor Foreground Vivid Yellow]
    >> putStrLn (unlines
        [
          "                           [ WELCOME TO VOLUNTEERING MANAGEMENT SYSTEM ]                           "
        , " .=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-."
        , " ||                                                                                              ||"
        , " ||         ██╗   ██╗ ██████╗ ██╗     ██╗   ██╗███╗   ██╗████████╗███████╗███████╗██████╗        ||"
        , " ||         ██║   ██║██╔═══██╗██║     ██║   ██║████╗  ██║╚══██╔══╝██╔════╝██╔════╝██╔══██╗       ||"
        , " ||         ██║   ██║██║   ██║██║     ██║   ██║██╔██╗ ██║   ██║   █████╗  █████╗  ██████╔╝       ||"
        , " ||         ╚██╗ ██╔╝██║   ██║██║     ██║   ██║██║╚██╗██║   ██║   ██╔══╝  ██╔══╝  ██╔══██╗       ||"
        , " ||          ╚████╔╝ ╚██████╔╝███████╗╚██████╔╝██║ ╚████║   ██║   ███████╗███████╗██║  ██║       ||"
        , " ||           ╚═══╝   ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚══════╝╚══════╝╚═╝  ╚═╝       ||"
        , " .=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-."
        , " ||   COLLABORATION   |     GROWTH     |      IMPACT      |    INNOVATION    |    LEADERSHIP     ||"
        , " .=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-."
        , "                                 Let's build a better world together!                              "
        ])
    >> setSGR [Reset]

-- Print ASCII Art Thank You Message
printThankYou :: IO ()
printThankYou = clearScreen
    >> setSGR [SetColor Foreground Vivid Yellow]
    >> putStrLn (unlines
    [ "                          [ THANK YOU FOR USING VOLUNTEERING MANAGEMENT SYSTEM ]                       "
    , " .=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-."
    , " ||                                                                                                  ||"
    , " ||          ████████╗██╗  ██╗ █████╗ ███╗   ██╗██╗  ██╗    ██╗   ██╗ ██████╗ ██╗   ██╗██╗           ||"
    , " ||          ╚══██╔══╝██║  ██║██╔══██╗████╗  ██║██║ ██╔╝    ╚██╗ ██╔╝██╔═══██╗██║   ██║██║           ||"
    , " ||             ██║   ███████║███████║██╔██╗ ██║█████╔╝      ╚████╔╝ ██║   ██║██║   ██║██║           ||"
    , " ||             ██║   ██╔══██║██╔══██║██║╚██╗██║██╔═██╗       ╚██╔╝  ██║   ██║██║   ██║╚═╝           ||"
    , " ||             ██║   ██║  ██║██║  ██║██║ ╚████║██║  ██╗       ██║   ╚██████╔╝╚██████╔╝██╗           ||"
    , " ||             ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝       ╚═╝    ╚═════╝  ╚═════╝ ╚═╝           ||"
    , " ||                                                                                                  ||"
    , " .=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-."
    , "                                Have a nice day and see you next time ^_^                             "
    ])
    >> setSGR [Reset]

-- Print Functions for Different Message Types
printMessage, printSuccess, printError :: String -> IO ()
printMessage message = setSGR [SetColor Foreground Vivid Cyan] >> putStrLn message >> setSGR [Reset]
printSuccess message = setSGR [SetColor Foreground Vivid Green] >> putStrLn message >> setSGR [Reset]
printError message = setSGR [SetColor Foreground Vivid Red] >> putStrLn message >> setSGR [Reset]

-- Menu Display Function
displayMenu :: MenuOption a => a -> String -> IO String
displayMenu menuType prompt = putStrLn prompt
    >> mapM_ (\(k, v) -> putStrLn $ k ++ "." ++ v) (getOptions menuType)
    >> getLine
    >>= \choice -> if validateOption menuType choice
        then return choice
        else printError ("\nInvalid choice! Please enter a number between 1 and " ++ show (length $ getOptions menuType) ++ "!")
            >> displayMenu menuType prompt

-- CSV operations
-- Convert Volunteer to CSV String
volunteerCSV :: Volunteer -> String
volunteerCSV v = intercalate ","
    [ volunteerID v
    , name v
    , contactInformation v
    , show (role v)
    , show (availability v)
    , show (hoursContributed v)
    , show (noOfEventsParticipated v)
    ]

-- Parse CSV line to Volunteer
parseCSVLine :: String -> Maybe Volunteer
parseCSVLine line = case words $ map (\b -> if b == ',' then  ' ' else b) line of
    [id, name, contact, role, availability, hours, events] ->
        Volunteer id name contact
        <$> readMaybe role
        <*> readMaybe availability
        <*> readMaybe hours
        <*> readMaybe events
    _ -> Nothing

-- Read Volunteer Data from CSV file
readVolunteerFromCSV :: IO [Volunteer]
readVolunteerFromCSV = catch
    (readFile "app/VolunteeringManagementSystem.csv")
    handleFileError
    >>= \content -> return $ foldr addValidVolunteer []
        (drop 1 $ filter (not . null) $ lines content)
    where
        handleFileError :: IOException -> IO String
        handleFileError e = writeFile "app/VolunteeringManagementSystem.csv" csvHeader
            >> return (fromMaybe "" $ Just csvHeader )
        addValidVolunteer line acc = case parseCSVLine line of
            Just volunteer -> volunteer : acc
            Nothing -> acc

-- Save Volunteer Data to CSV file
saveVolunteerToCSV :: [Volunteer] -> IO ()
saveVolunteerToCSV volunteers = do
    let temporaryFile = "app/temporary.csv"
    withFile temporaryFile WriteMode $ \handle -> do
        hPutStrLn handle csvHeader
        mapM_ (hPutStrLn handle . volunteerCSV) volunteers
    renameFile temporaryFile "app/VolunteeringManagementSystem.csv"

-- CSV Header
csvHeader :: String
csvHeader = "VolunteerID,Name,ContactInformation,Role,Availability,HoursContributed,EventsParticipated"

-- Check if the Input is Not Empty
isNotEmpty :: String -> Bool
isNotEmpty = not . null

-- Check if the Input Contains Only Digits
isAllDigits :: String -> Bool
isAllDigits = all (`elem` ['0' .. '9'])

-- Check if the Input Contains Only Letters
isAllLetters :: String -> Bool
isAllLetters = all (`elem` ([' '] ++ ['a' .. 'z'] ++ ['A' .. 'Z']))

-- Check if the Input is Positive Number
isPositiveNumber :: (Read a, Ord a, Num a) => String -> Maybe a
isPositiveNumber str = case readMaybe str of 
    Just n | n >= 0 -> Just n
    _ -> Nothing

-- Validation Functions
-- Validate Volunteer ID
validateID :: String -> Validation String
validateID id =
    if isNotEmpty id && length id == 5 && isAllDigits id
        then pure id
        else Failure "Invalid Volunteer ID! Please enter a Volunteer ID with 5-digits!"

-- Validate Name
validateName :: String -> Validation String
validateName name =
    if isNotEmpty name && isAllLetters name
        then Success (map toLower name)
            <&> words
            <&> map toTitleCase
            <&> unwords
    else Failure "Invalid name! Please enter a name with letters only!"

-- Validate Contact Information
validateContact :: String -> Validation String
validateContact contact =
    if isNotEmpty contact && length contact `elem` [10, 11] && take 2 contact == "01" && isAllDigits contact
        then pure contact
        else Failure "Invalid contact information! Please enter a contact information starting with '01' (10 to 11-digits)!"

-- Validate Role
validateRole :: String -> Validation Role
validateRole str =
    case map toLower str of
        s -> Success (map toLower s)
            <&> toTitleCase
            >>= (maybe
                    (Failure "Invalid role! Please enter again!")
                    Success . readMaybe)

-- Validate Availability
validateAvailability :: String -> Validation Availability
validateAvailability str =
    case map toLower str of
        s -> Success (map toLower s)
            <&> toTitleCase
            >>= (maybe
                    (Failure "Invalid availability! Please enter again!")
                    Success . readMaybe)

-- Validate Hours Contributed
validateHours :: String -> Validation Double
validateHours str = 
    case isPositiveNumber str of
    Just hours -> pure hours
    Nothing -> Failure "Invalid hours contributed! Please enter hours contributed with positive number!"

-- Validate Number of Events Participated
validateEvents :: String -> Validation Int
validateEvents str = 
    case isPositiveNumber str of
    Just events -> pure events
    Nothing -> Failure  "Invalid number of events participated! Please enter number of events participated with positive number!"

-- Helper Function for Title Case Conversion
toTitleCase :: String -> String
toTitleCase [] = []
toTitleCase (x:xs) = toUpper x:xs

-- Input Volunteer Information Validation
inputInformation :: (String -> Validation a) -> String -> IO (Maybe a)
inputInformation validator prompt = putStrLn prompt
    >> getLine
    >>= \input -> case validator input of
        Success value -> return (Just value)
        Failure err -> printError err
            >> inputInformation validator prompt

-- Input Volunteer ID with Validation
inputVolunteerID :: [Volunteer] -> Operation -> IO (Maybe String)
inputVolunteerID volunteers op = inputInformation validateID "\nEnter Volunteer ID (5-digits): "
    >>= \maybeValidID -> case maybeValidID of
        Nothing -> return Nothing
        Just validID ->
            (\exists -> if (op == Add && not exists) || (op /= Add && exists)
                then return (Just validID)
                else printError (if op == Add
                    then "Volunteer ID is already existing! Please enter again!"
                    else "Volunteer ID is not found. Please enter again!")
                    >> inputVolunteerID volunteers op)
            (any (\v -> volunteerID v == validID) volunteers)

-- Collect all volunteer inputs
getVolunteerInputs :: String -> IO (Maybe Volunteer)
getVolunteerInputs id =
    inputInformation validateName "\nEnter name: "
    >>= \maybeName -> inputInformation validateContact "\nEnter contact information (10 to 11-digits): "
    >>= \maybeContact -> inputInformation validateRole "\nEnter role (Community/Environment/Health/Wildlife): "
    >>= \maybeRole -> inputInformation validateAvailability "\nEnter availability (Available/Unavailable): "
    >>= \maybeAvailability -> inputInformation validateHours "\nEnter hours contributed: "
    >>= \maybeHours -> inputInformation validateEvents "\nEnter number of events participated: "
    >>= \maybeEvents -> return $ Volunteer id
        <$> maybeName
        <*> maybeContact
        <*> maybeRole
        <*> maybeAvailability
        <*> maybeHours
        <*> maybeEvents

-- Confirmation
confirmation :: IO Bool
confirmation = putStrLn "Enter 'y' for Yes or 'n' for No: "
    >> map toLower . filter (/= ' ') <$> getLine
    >>= \choice -> case choice of
        "y" -> return True
        "n" -> return False
        _ -> printError "\nInvalid input! Please enter either 'y' or 'n'!"
            >> confirmation

-- Show Volunteer Information
showVolunteerInformation :: Volunteer -> IO ()
showVolunteerInformation v = putStrLn "\nVolunteer Information: "
    >> mapM_ putStrLn
        [replicate 40 '-',
        "Volunteer ID: " ++ volunteerID v,
        "Name: " ++ name v,
        "Contact information: " ++ contactInformation v,
        "Role: " ++ show (role v),
        "Availability: " ++ show (availability v),
        "Hours contributed: " ++ show (hoursContributed v),
        "Number of events participated: " ++ show (noOfEventsParticipated v),
        replicate 40 '-']

-- Get Field Input
getFieldInput :: String -> String -> IO String
getFieldInput fieldName prompt = putStrLn prompt
    >> putStrLn ("\nEnter new " ++ fieldName ++ ":")
    >> getLine

-- Edit Field
editField :: Volunteer -> [Volunteer] -> IO (Maybe (Volunteer, Bool))
editField volunteer volunteers = displayMenu EditMenu "\nPlease choose the information to edit: "
    >>= \editChoice -> getFieldValue editChoice
    where
        getFieldValue editChoice = getFieldInput
            (maybe "" snd (find ((== editChoice) . fst) (getOptions EditMenu))) ""
            >>= \input -> case editChoice of
                "1" -> validateAndUpdate validateName (\new v -> v {name = new}) name input
                "2" -> validateAndUpdate validateContact (\new v -> v {contactInformation = new}) contactInformation input
                "3" -> validateAndUpdate validateRole (\new v -> v {role = new}) role input
                "4" -> validateAndUpdate validateAvailability (\new v -> v {availability = new}) availability input
                "5" -> validateAndUpdate validateHours (\new v -> v {hoursContributed = new}) hoursContributed input
                "6" -> validateAndUpdate validateEvents (\new v -> v {noOfEventsParticipated = new}) noOfEventsParticipated input
                _ -> return Nothing
            >>= \maybeUpdated -> case maybeUpdated of
                Nothing -> getFieldValue editChoice
                Just updatedVolunteer -> putStrLn "\nWould you like you edit another field of information for this volunteer?"
                    >> confirmation
                    >>= \editAnotherField -> if editAnotherField
                        then editField updatedVolunteer volunteers
                        else return $ Just (updatedVolunteer, True)

        validateAndUpdate :: (Eq a) => (String -> Validation a)
                        -> (a -> Volunteer -> Volunteer)
                        -> (Volunteer -> a)
                        -> String
                        -> IO (Maybe Volunteer)
        validateAndUpdate validator updater getter input =
            case validator input of
                Failure err -> printError err >> return Nothing
                Success newInformation
                    | newInformation == getter volunteer ->
                        printError "This is a current information! Please enter a different information!"
                        >> return Nothing
                    | otherwise -> return $ Just $ updater newInformation volunteer

-- Padding Function for Table Display
padRight :: Int -> String -> String
padRight n str = take n (str ++ repeat ' ')

-- Display Table Header
displayHeader :: IO ()
displayHeader = mapM_ putStrLn
    [replicate 110 '=',
     "|" ++ intercalate "|"
       [padRight 6 "ID",
        padRight 15 "Name",
        padRight 13 "Contact Info",
        padRight 13 "Role",
        padRight 13 "Availability",
        padRight 17 "Hours Contributed",
        padRight 25 "No of Events Participated"] ++ "|",
    replicate 110 '=']

-- Display Volunteer Information in Row
displayVolunteerRow :: Volunteer -> IO ()
displayVolunteerRow v = putStrLn $ "|" ++ intercalate "|"
    [padRight 6 (volunteerID v),
     padRight 15 (name v),
     padRight 13 (contactInformation v),
     padRight 13 (show $ role v),
     padRight 13 (show $ availability v),
     padRight 17 (show $ hoursContributed v),
     padRight 25 (show $ noOfEventsParticipated v)] ++ "|"

-- Perform Specific Operation
performOperation :: Operation -> (Volunteer -> IO()) -> [Volunteer] -> (Volunteer -> [Volunteer] -> IO [Volunteer]) -> IO [Volunteer]
performOperation op operation volunteers action =
    inputVolunteerID volunteers op
        >>= \maybeValidID -> case maybeValidID of
            Nothing-> return volunteers
            Just validID -> case find (\v -> volunteerID v == validID) volunteers of
                Nothing -> return volunteers
                Just volunteer -> operation volunteer
                    >> action volunteer volunteers

-- Return Sorting Function Based on the User Choice
getSortFunction :: String -> Maybe (Volunteer -> String)
getSortFunction choice = case choice of
    "1" -> Just volunteerID
    "2" -> Just name
    "3" -> Just contactInformation
    "4" -> Just (show . hoursContributed)
    "5" -> Just (show . noOfEventsParticipated)
    _ -> Nothing

-- Return Ordering Function Based on the User Choice
getOrderFunction :: (Volunteer -> String) -> String -> [Volunteer] -> [Volunteer]
getOrderFunction sortFunction orderChoice =
    fromMaybe id $ lookup orderChoice
        [("1" , sortOn sortFunction)
        ,("2" , sortOn (Data.Ord.Down . sortFunction))]

-- Return Role Value Based on the User Choice
getRole :: String -> Role
getRole choice = case choice of
    "1" -> Community
    "2" -> Environment
    "3" -> Health
    "4" -> Wildlife

-- Return Availability Value Based on the User Choice
getAvailability :: String -> Availability
getAvailability choice = case choice of
    "1" -> Available
    "2" -> Unavailable

-- Return Filtering Function Based on the User Choice
getFilterFunction :: String -> [Volunteer] -> String -> [Volunteer]
getFilterFunction filterChoice volunteers = case filterChoice of
    "1" -> \roleChoice -> filter (\v -> role v == getRole roleChoice) volunteers
    "2" -> \availabilityChoice -> filter (\v -> availability v == getAvailability availabilityChoice) volunteers
    _ -> const volunteers

-- Return a Report Based on the User Choice
getReport :: String -> Report
getReport choice = case choice of
    "1" -> roleReport
    "2" -> availabilityReport
    "3" -> topVolunteerReport
    "4" -> statusReport
    "5" -> participationReport
    _ -> mempty

-- Volunteers by Role Report
roleReport :: Report
roleReport = Report "" $ \volunteers ->
    "\nReport: Volunteers by Role" :
    map (\r -> "Total Volunteers of " ++ show r ++ ": " ++
        show (length $ filter (\v -> role v == r) volunteers))
        [Community, Environment, Health, Wildlife]

-- Volunteers by Availability Report
availabilityReport :: Report
availabilityReport = Report "" $ \volunteers ->
     ["\nReport: Volunteers by Availability",
     "Number of Available Volunteers: " ++ show (length $ filter (\v -> availability v == Available) volunteers),
     "Number of Unavailable Volunteers: " ++ show (length $ filter (\v -> availability v == Unavailable) volunteers)]

-- Top Volunteers by Contributed Hours Report
topVolunteerReport :: Report
topVolunteerReport = Report "" $ \volunteers ->
    "\nReport: Top Volunteers by Contributed Hours":
    concatMap (\v -> [name v,
                      "Hours Contributed: " ++ show (hoursContributed v),
                      "Number of Events Participated: " ++ show (noOfEventsParticipated v), ""])
    (take 3 $ sortOn (Data.Ord.Down . hoursContributed) volunteers)

-- Volunteer Status Report
statusReport :: Report
statusReport = Report "" $ \volunteers ->
    ["\nReport: Volunteer Status",
     "Number of Active Volunteers: " ++ show (length $ filter (\v -> hoursContributed v > 0) volunteers),
     "Number of Inactive Volunteers: " ++ show (length $ filter (\v -> hoursContributed v == 0) volunteers)]

-- Volunteers Participation Report
participationReport :: Report
participationReport = Report "" $ \volunteers ->
    ["\nReport: Volunteers Participation Summary",
     "Total Volunteers: " ++ show (length volunteers),
     "Total Volunteer Hours: " ++ show (sum $ map hoursContributed volunteers),
     "Total Events Participated: " ++ show (sum $ map noOfEventsParticipated volunteers)]
     
-- Add Volunteer Using State Monad
addVolunteerState :: Volunteer -> VolunteerState ()
addVolunteerState = State.modify . (:)

-- Delete Volunteer Using State Monad
deleteVolunteerState :: String -> VolunteerState ()
deleteVolunteerState id = State.modify $ filter (\v -> volunteerID v /= id)

-- Edit Volunteer Using State Monad
editVolunteerState :: Volunteer -> VolunteerState ()
editVolunteerState updatedVolunteer = State.modify $ map (\v ->
    if volunteerID v == volunteerID updatedVolunteer
    then updatedVolunteer
    else v)

-- Get Volunteer by ID Using State Monad
getVolunteerByIDState :: String -> VolunteerState (Maybe Volunteer)
getVolunteerByIDState id = State.gets $ find (\v -> volunteerID v == id)

-- Add Volunteer Information
addVolunteerInformation :: [Volunteer] -> IO [Volunteer]
addVolunteerInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|    Add New Volunteer Information     |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> inputVolunteerID volunteers Add
    >>= \maybeValidID -> case maybeValidID of
        Nothing-> return volunteers
        Just validID -> getVolunteerInputs validID
            >>= \maybeNewVolunteer -> case maybeNewVolunteer of
                Nothing -> return volunteers
                Just newVolunteer -> showVolunteerInformation newVolunteer
                    >> putStrLn "\nDo you confirm to add new volunteer information?"
                    >> confirmation
                    >>= \confirmed -> (if confirmed
                        then printSuccess "\nCongratulations! Volunteer information is added successfully!"
                            >> saveVolunteerToCSV (execState (addVolunteerState newVolunteer) volunteers)
                            >> return (execState (addVolunteerState newVolunteer) volunteers)
                        else printError "\nCancel adding volunteer information."
                            >> saveVolunteerToCSV volunteers
                            >> return volunteers)

                    >>= \updatedVolunteers -> putStrLn "\nDo you want to add another volunteer information?"
                        >> confirmation
                    >>= \addMore -> if addMore
                        then addVolunteerInformation updatedVolunteers
                        else return updatedVolunteers

-- Delete Volunteer Information
deleteVolunteerInformation :: [Volunteer] -> IO [Volunteer]
deleteVolunteerInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|     Delete Volunteer Information     |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> performOperation Delete showVolunteerInformation volunteers volunteerDelete
    where
        volunteerDelete :: Volunteer -> [Volunteer] -> IO [Volunteer]
        volunteerDelete volunteer volunteers = putStrLn "\nDo you confirm to delete this volunteer information?"
            >> confirmation
            >>= \confirmed ->
                (if confirmed
                then printSuccess "\nVolunteer information is deleted successfully!"
                    >> saveVolunteerToCSV (execState (deleteVolunteerState (volunteerID volunteer)) volunteers)
                    >> return (execState (deleteVolunteerState (volunteerID volunteer)) volunteers)
                else printError "\nCancel deleting volunteer information."
                    >> saveVolunteerToCSV volunteers
                    >> return volunteers)

            >>= \updatedVolunteers -> putStrLn "\nDo you want to delete another volunteer information?"
                >> confirmation
                >>= \deleteMore -> if deleteMore
                    then deleteVolunteerInformation updatedVolunteers
                    else return updatedVolunteers

-- Search Volunteer Information
searchVolunteerInformation :: [Volunteer] -> IO [Volunteer]
searchVolunteerInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|     Search Volunteer Information     |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> performOperation Search showVolunteerInformation volunteers volunteerSearch
    where
        volunteerSearch _ volunteers = putStrLn "\nDo you want to search another volunteer information?"
            >> confirmation
            >>= \searchMore -> if searchMore
                then searchVolunteerInformation volunteers
                else return volunteers

-- Edit Volunteer Information
editVolunteerInformation :: [Volunteer] -> IO [Volunteer]
editVolunteerInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|      Edit Volunteer Information      |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> performOperation Edit showVolunteerInformation volunteers volunteerEdit
    where
        volunteerEdit volunteer volunteers = editField volunteer volunteers
            >>= \maybeResult -> case maybeResult of
                Nothing -> return volunteers
                Just (updatedVolunteer, _) -> putStrLn "\nUpdated volunteer information: "
                    >> showVolunteerInformation updatedVolunteer
                    >> putStrLn "\nDo you confirm want to update this volunteer information?"
                    >> confirmation
                    >>= \confirmed -> (if confirmed
                        then printSuccess "\nCongratulations! Volunteer information is updated successfully!"
                        >> saveVolunteerToCSV (execState (editVolunteerState updatedVolunteer) volunteers)
                        >> return (execState (editVolunteerState updatedVolunteer) volunteers)
                        else printError "\nCancel editing volunteer information."
                        >> saveVolunteerToCSV volunteers
                        >> return volunteers)

                    >>= \finalVolunteers -> putStrLn "\nDo you want to edit another volunteer information?"
                        >> confirmation
                        >>= \editMore -> if editMore
                            then editVolunteerInformation finalVolunteers
                            else return finalVolunteers
                    where
                        updatedVolunteers = map (\v -> if volunteerID v == volunteerID volunteer
                                                then updatedVolunteer
                                                else v) volunteers

-- Display All Volunteers Information 
displayAllVolunteersInformation :: [Volunteer] -> IO [Volunteer]
displayAllVolunteersInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|   Display All Volunteers Information   |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> (if null volunteers
        then putStrLn "\nNo volunteers are found in the system!"
        else displayHeader
            >> mapM_ displayVolunteerRow volunteers
            >> putStrLn (replicate 110 '='))
    >> putStrLn "Press 'enter' or input anything to continue..."
    >> getLine
    >> return volunteers

-- Sort Volunteer Information
sortVolunteerInformation :: [Volunteer] -> IO [Volunteer]
sortVolunteerInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|     Sort Volunteer Information       |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> displayMenu SortMenu "\nPlease choose the sorting criteria: "
    >>= \sortChoice -> displayMenu OrderMenu "\nPlease choose the sorting order: "
    >>= \orderChoice -> case getSortFunction sortChoice of
        Nothing -> return volunteers
        Just sortFunction -> displayAllVolunteersInformation (getOrderFunction sortFunction orderChoice volunteers)
            >> putStrLn "\nDo you want to sort again with different criteria?"
            >> confirmation
            >>= \sortAgain -> if sortAgain
                then sortVolunteerInformation volunteers
                else return volunteers

-- Filter Volunteer Information
filterVolunteerInformation :: [Volunteer] -> IO [Volunteer]
filterVolunteerInformation volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|     Filter Volunteer Information     |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> displayMenu FilterMenu "\nPlease choose the filter criteria: "
    >>= \filterChoice -> ((case filterChoice of
        "1" -> displayMenu RoleMenu "\nPlease choose the role to filter: "
            >>= \roleChoice -> return $ getFilterFunction filterChoice volunteers roleChoice
        "2" -> displayMenu AvailabilityMenu "\nPlease choose the availability to filter: "
            >>= \availabilityChoice -> return $ getFilterFunction filterChoice volunteers availabilityChoice
        _ -> return volunteers)
    >>= displayAllVolunteersInformation) >> (putStrLn "\nDo you want to filter again with different criteria?"
        >> confirmation
        >>= \filterAgain -> if filterAgain
        then filterVolunteerInformation volunteers
        else return volunteers)

-- Generate Report Summary 
generateReportSummary :: [Volunteer] -> IO [Volunteer]
generateReportSummary volunteers = clearScreen
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> printMessage "|           Report Summary             |"
    >> printMessage "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    >> displayMenu ReportMenu "\nPlease choose the report to view: "
    >>= \reportChoice -> mapM_ putStrLn (reportContent (getReport reportChoice) volunteers)
        >> putStrLn "\nDo you want to view this report together with another report?"
        >> confirmation
        >>= \combineReport -> if combineReport
            then chooseSecondReport reportChoice volunteers
            else continuePrompt volunteers
    where
        chooseSecondReport firstChoice volunteers = displayMenu ReportMenu "\nPlease choose another report to view together: "
            >>= \secondChoice -> if secondChoice == firstChoice
                then printError "\nInvalid choice! Please choose a different report!"
                    >> chooseSecondReport firstChoice volunteers
                else putStrLn "\nCombined Report:"
                        >> mapM_ putStrLn (reportContent (getReport firstChoice <> getReport secondChoice) volunteers)
                        >> continuePrompt volunteers

        continuePrompt volunteers = putStrLn "\nDo you want to view another report?"
            >> confirmation
            >>= \viewOtherReport -> if viewOtherReport
            then generateReportSummary volunteers
        else return volunteers

-- Main Function
main :: IO ()
main = readVolunteerFromCSV
    >>= \initialVolunteers -> menuLoop initialVolunteers
    >> printThankYou

-- Main Menu Loop
menuLoop :: [Volunteer] -> IO [Volunteer]
menuLoop volunteers = clearScreen
    >> printAsciiArt
    >> displayMenu MainMenu "\nVolunteering Management System"
    >>= \choice -> case choice of
        "1" -> addVolunteerInformation volunteers >>= menuLoop
        "2" -> deleteVolunteerInformation volunteers >>= menuLoop
        "3" -> searchVolunteerInformation volunteers >>= menuLoop
        "4" -> editVolunteerInformation volunteers >>= menuLoop
        "5" -> displayAllVolunteersInformation volunteers >>= menuLoop
        "6" -> sortVolunteerInformation volunteers >>= menuLoop
        "7" -> filterVolunteerInformation volunteers >>= menuLoop
        "8" -> generateReportSummary volunteers >>= menuLoop
        "9" -> return volunteers
        _ -> printError "Invalid choice! Please enter again!"
            >> menuLoop volunteers