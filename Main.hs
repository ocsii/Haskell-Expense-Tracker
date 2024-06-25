-- Expense tracker
module Main where

import Data.List (groupBy, sortBy, find)
import qualified Data.Map.Strict as Map 
import Text.CSV
import Text.Printf -- To round and format values


-- ===============================================================================================================
-- General fucntions and Data types

-- Data type to store expense records
data ExpenseRecord = ExpenseRecord {number :: Int, name :: String, value :: Float, category :: Category} deriving (Show)

-- Type alias for expense info (individual information of records)
type ExpenseInfo = (Int, String, Float, Category)

-- Data type for category (Derive Ord Eq to sort by category)
data Category = Food | Leisure | Transportation | Family | Other | Invalid deriving (Ord, Eq)

-- Show instance of Category (To print)
instance Show Category where
  show Food = "🍎 Food"
  show Leisure = "🎥 Leisure"
  show Transportation = "🚗 Transportation"
  show Family = "👪 Family"
  show Other = "💡 Others"
  show Invalid = "Invalid"


-- Mapping of category to number / Categories are stored in CSV as Int Numbers 
-- Convert number to category (unparse)
getCategoryName :: Int -> Category
getCategoryName category = case category of
    1 -> Food
    2 -> Leisure
    3 -> Transportation
    4 -> Family
    5 -> Other
    _ -> Invalid


-- Convert category to number (to store in CSV) 
getCategoryNumber :: Category -> Int
getCategoryNumber category = case category of
  Food -> 1
  Leisure -> 2
  Transportation -> 3
  Family -> 4
  Other -> 5
  _ -> 0


-- Read CSV and create list of ExpenseRecords
csvToExpense :: CSV -> [ExpenseRecord]
csvToExpense csvFile = fmap (\record -> ExpenseRecord{number = read $ record !! 0, name = record !! 1, value = read $ record !! 2, category = getCategoryName $ read $ record !! 3 }) csvFile


-- Convert expense record to CSV record to be able to write into CSV file
expenseToCSV :: ExpenseRecord -> Record
expenseToCSV expenseRecord = [show $ number expenseRecord, name expenseRecord, show $ value expenseRecord, show $ getCategoryNumber $ category expenseRecord]


-- To get the next ID number
getNextNumber :: IO Int
getNextNumber = do
  records <- readAllRecords 

  return $ last (0 : [number x | x <- records ]) + 1 -- Add 0 if list is empty


-- Format the record by concat them with (,) and adding identifier
formatRecord :: ExpenseRecord -> IO String
formatRecord expenseRecord = do 

  -- Get the next number before appending and add to expense record
  nextNumber <- getNextNumber 
  let newExpenseRecord = expenseRecord {number = nextNumber}

  -- Convert ExpenseRecord to Record and combine it usign (,)
  let record = expenseToCSV newExpenseRecord 
      formattedRecord = init (concatMap (<> ",") record ) <> "\n" -- Remove last comma

  return formattedRecord


-- Write expenses to CSV (Append so it does not rewrite)
appendCSV :: ExpenseRecord -> IO ()
appendCSV expenseRecord = do

  formattedRecord <- formatRecord expenseRecord

  appendFile "expenses.csv" formattedRecord 


-- Function to read CSV file
readAllRecords :: IO [ExpenseRecord]
readAllRecords = do
  files <- parseCSVFromFile "expenses.csv"
  case files of  
    Left err -> print err >> return []
    Right csv -> return $ csvToExpense $ init $ tail csv


-- ===============================================================================================================
-- Process 1 -- Add expenses

-- Function for validation (Maybe and just)
-- If reads success return -> (value, "") if not success return (0, "value")
readMaybe :: Read a => String -> Maybe a
readMaybe numberString = case reads numberString of
  [(x, "")] -> Just x -- If Value,  return x
  _         -> Nothing 


-- Get input from user
getExpenseInfo :: IO ExpenseRecord
getExpenseInfo = do

  putStr "Enter expense name: " 
  name <- getLine
  
  value <- getValue
  category <- getCategory

  -- ID is set to 0 first and formatted when appending 
  return $ ExpenseRecord 0 name value $ getCategoryName category 


-- Get value of input
getValue :: IO Float
getValue = do
  putStr "\nEnter expense amount (RM): "
  valueString <- getLine

  -- Validation
  let value = readMaybe valueString :: Maybe Float
  case value of
    Just v | v > 0  -> return v
    Just v | v <= 0 -> putStrLn "*** Cannot be negative or 0, please try again ***" >> getValue
    _               -> putStrLn "*** Invalid input. Please enter a NUMBER in (RM) ***" >> getValue


-- Get category of input
getCategory :: IO Int
getCategory = do  
  putStr "\n1. " <> print Food
  putStr "2. " <> print Leisure
  putStr "3. " <> print Transportation
  putStr "4. " <> print Family
  putStr "5. " <> print Other
  putStr "\nEnter Category (Number): "
  categoryString <- getLine

  -- Validation
  let category = readMaybe categoryString :: Maybe Int
  case category of
    Just c | c <= 5 && c >= 1 -> return c -- Check if input is within 1 - 5
    _                         -> putStrLn "*** Invalid category. Please select a valid category from (1 -> 5) ***" >> getCategory


-- Main function to add expenses
addExpense :: IO ()
addExpense = do
  putStrLn "\n------------------ Adding Expense -------------------\n"

  -- Get info and append to file
  expenseInfo <- getExpenseInfo
  appendCSV expenseInfo

  putStrLn $ "\nExpense succesfully added ✅ \n  Name    : " <> name expenseInfo <> 
             "\n  Value   : " <> show (value expenseInfo) <>
             "\n  Category: " <> show (category expenseInfo)


-- ==============================================================================================================
-- Process 2 Display all expenses

-- Extract each line from record
extractExpenseInfo :: [ExpenseRecord] -> [ExpenseInfo]
extractExpenseInfo records = [(number record, name record, value record, category record) | record <- records]


-- Print indivual expense information
printExpenseInfo :: ExpenseInfo -> IO ()
printExpenseInfo (number, name, value, category) = printf "%-5d %-20s  %10.2f        %-10s\n" number name value $ show category


-- Print all expenses in a table
printAllExpenses :: IO ()
printAllExpenses = do
  records <- readAllRecords

  -- Headers
  putStrLn "\n------------- All Expenses Listed Below -------------\n"
  printf "%-5s %-20s %10s       %-10s\n\n" "ID**" "Name**" "Value (RM)**" "Category**"

  mapM_ printExpenseInfo $ extractExpenseInfo records -- Individual records (Discard mapped values)

  printf "\n%-25s   %10.2f\n" "Combined total:" $ getTotal records -- Print total value
  

-- ===============================================================================================================
-- Process 3 Group by category

-- Get sum of values for combined total 
getTotal :: [ExpenseRecord] -> Float
getTotal records = foldr (\record total -> value record + total) 0 records


-- Group records based on category and add all values together (Category Int, Total value)
groupByCategory :: [ExpenseRecord] -> [(Category, Float)]
groupByCategory records = Map.toList $ Map.fromListWith (+) [(category record, value record) | record <- records]


-- Print summary of individual category
printCategory :: (Category, Float) -> IO ()
printCategory (category, total) = printf "%-20s %10.2f\n" (show category) total


-- Print based on category
printByCategory :: IO ()
printByCategory = do
  records <- readAllRecords -- Unwrap IO

  -- Headers
  putStrLn "\n---------------- Summary by Category ----------------\n"
  printf "%-20s %10s\n\n" "Category**" "Totals (RM)**"
  
  mapM_ printCategory $ groupByCategory records -- Individual categories (Discard mapped values)

  printf "\n%-20s  %10.2f \n" "Combined total:" $ getTotal records -- Print total value


-- ===============================================================================================================
-- Process 4 - Delete expenses

-- Wipe all expenses and add header only
clearCSV :: IO ()
clearCSV = writeFile "expenses.csv" "Expense_Number,Expense_Name,Expense_Value,Expense_Category\n"


-- Used to get ID for deletion and modification (takes process name as process function)
getID :: String -> (String -> IO ()) -> IO ()
getID processName function = do

  -- Get input of ID to edit
  printf "\nEnter the ID of the expense you want to %s: " processName 
  targetExpense <- getLine

  -- Validation
  let readTarget = readMaybe targetExpense :: Maybe Int

  case readTarget of
    Just _  -> function targetExpense
    Nothing -> putStrLn "Invalid input. Please enter a valid ID" >> getID processName function


-- Delete the expense from the ExpenseRecord list and rewrite entire thing to CSV
deleteExpense :: String -> IO ()
deleteExpense targetExpense = do
  records <- readAllRecords -- Unwrap IO

  -- Extract the targetedExpense by matching pattern
  case find (\record -> number record == read targetExpense) records of
    Just _ -> do
    
      -- Filter out / remove the targetExpense
      let updatedRecords = filter (\record -> number record /= read targetExpense) records
    
      -- Clear CSV and reappend everything (append function resets the ID numbers)
      clearCSV         
      mapM_ appendCSV updatedRecords
      putStrLn $ "\nExpense number '" <> targetExpense <> "' succesfully deleted ✅"
          
    Nothing -> putStrLn "\nExpense not found." -- Expense ID does not exist
        

-- Get ID to delete expense
deleteMenu :: IO ()
deleteMenu = do 

  printAllExpenses -- Print expenses for user to choose from 

  getID "DELETE" deleteExpense


-- ===============================================================================================================
-- Process 5 - Modify expenses

-- Replace the targeted expense with new values
editExpense :: String -> IO ()
editExpense targetExpense = do
  records <- readAllRecords -- Unwrap IO
  
  -- Extract the targetedExpense by matching pattern
  case find (\record -> number record == read targetExpense) records of
    Just expenseToEdit -> do
    
      -- Display current expense being edited
      putStrLn "\n---------------- Expense being edited ---------------\n"
      printf "%-5s %-20s %10s       %-10s\n\n" "ID" "Name**" "Value (RM)**" "Category**"
      printExpenseInfo (number expenseToEdit, name expenseToEdit, value expenseToEdit, category expenseToEdit)
    
      -- Get new vlues
      putStrLn "\nEnter the NEW values\n"
      updatedExpense <- getExpenseInfo
    
      -- Find the expense and replace it with updated expense
      let updatedRecords = fmap (\record -> if number record == read targetExpense then updatedExpense else record) records
    
      -- Clear CSV and reappend everything
      clearCSV
      mapM_ appendCSV updatedRecords
      putStrLn $ "\nExpense number '" <> targetExpense <> "' succesfully edited ✅"
          
    Nothing -> putStrLn "\nExpense not found" -- Expense ID does not exist

      
-- Get ID to modify expense
editMenu :: IO ()
editMenu = do

  printAllExpenses

  getID "MODIFY" editExpense

-- ===============================================================================================================
-- Main method -- Menu -- Process 

-- Main menu
mainMenu :: IO String
mainMenu = 
 putStrLn "\n================== Expense Tracker ==================\n" 
 <> putStrLn "1. ➕ Add expense" 
 <> putStrLn "2. 👁️  View All Expenses" 
 <> putStrLn "3. 🔠 View Expenses by Category" 
 <> putStrLn "4. 🗑️  Delete Expenses" 
 <> putStrLn "5. ✏️  Edit Expenses" 
 <> putStrLn "6. ⛔ Exit" 
 <> putStr "\nEnter choice (1 -> 6): " 
 >> getLine 
 

-- Process
process :: String -> IO ()
process "1" = addExpense >> mainMenu >>= process
process "2" = printAllExpenses >> mainMenu >>= process
process "3" = printByCategory >> mainMenu >>= process
process "4" = deleteMenu >> mainMenu >>= process
process "5" = editMenu >> mainMenu >>= process
process "6" = putStrLn "Goodbye!"
process _   = putStrLn "\n*** Invalid Choice! Please try again. ***" >> mainMenu >>= process


-- Main method
main :: IO ()
main = mainMenu >>= process