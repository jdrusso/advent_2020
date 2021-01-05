import System.IO 
import Data.List.Split
import Data.Maybe

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

    let result = fromJust ( sum_to_val digits 2020 )
    
    print result
    print (product result)

    hClose handle