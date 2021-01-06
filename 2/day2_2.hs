import System.IO 
import Data.List.Split

is_compliant :: String -> Bool
is_compliant policy_line =
    sum (map fromEnum is_pos_pw_char) == 1
    where 
        zipped_list = zip (words policy_line) [0..]
        (pos1:pos2:rest1) = head [map read (splitOn "-" rules) :: [Int] | (rules, idx) <- zipped_list, idx == 0]
        (pw_char:rest2) = head [pw_char | (pw_char, idx) <- zipped_list, idx == 1]
        pw = head [pw_char | (pw_char, idx) <- zipped_list, idx == 2]
        is_pos_pw_char = [letter == pw_char | (letter, letter_idx) <-  zip pw [1..], letter_idx == pos1 || letter_idx == pos2 ]

main = do
    handle <- openFile "day2_input" ReadMode
    contents <- hGetContents handle 
    
    let num_compliant = sum $ map fromEnum [is_compliant x | x <- lines contents]

    print num_compliant

    hClose handle