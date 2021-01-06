import Data.Matrix
import System.IO

find_trees :: Int -> Matrix Char -> Int
find_trees slope map_matrix = 
    let y_length = nrows map_matrix 
        x_length = ncols map_matrix
        steps_to_exit = y_length 
        x_indices = map (*slope) [1..steps_to_exit]
        x_indices_pbc = map (+1) $ map ( `mod` (x_length)) x_indices
        y_indices = [2..steps_to_exit]
        indices = zip   y_indices x_indices_pbc
        visited_points = [map_matrix ! idx | idx <- indices]
    in 
        sum [1 | point <- visited_points, point == '#']


main = do
    handle <- openFile "day3_input" ReadMode
    contents <- hGetContents handle 

    let x_length = length . head $ lines contents
    let y_length = length $ lines contents

    -- Build the array representation of the map
    let map_array = fromList y_length x_length [c | c <- contents, c /= '\n']

    print $ find_trees 3 map_array

    hClose handle