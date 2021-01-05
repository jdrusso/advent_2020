import System.IO 
import Data.List.Split
import Data.Maybe

-- A nested version of sum_to_val.
-- Day 1, part 2's challenge is to find 3 numbers that sum to 2020.
-- So, loop through the list in the same way, but look for pairs that sum to 2020-first
double_sum:: [Int] -> Int -> Maybe [Int]
double_sum list value =
    let step l value = case l of 
            [] -> Nothing 
            (x:xs) ->
                if not $ isNothing $ sum_to_val xs (value - x)
                    then  Just $ concat [[x], fromJust (sum_to_val xs (value - x))]
                    else step xs value 
    in step list value

sum_to_val:: [Int] -> Int -> Maybe [Int]
sum_to_val list value =
    let step l value = case l of 
            [] -> Nothing
            (x:xs) ->
                if length (filter (\z -> z + x == value) xs) == 1 
                    then Just [head (filter (\z -> z + x == value) xs), x]
                    else step xs value
    in step list value

main = do
    handle <- openFile "day1_input" ReadMode
    contents <- hGetContents handle 

    -- Read in the list of digits, split on \n, and convert to integers
    let digits = map read (words contents) :: [Int]

    let result = fromJust ( double_sum digits 2020 )
    -- let result =  ( double_sum digits 2020 )
    
    print result
    print (product result)

    hClose handle