import Data.Matrix
import System.IO

find_trees :: Matrix Char -> Int -> Int -> Int
find_trees map_matrix slope_x slope_y  = 
    let y_length = nrows map_matrix 
        x_length = ncols map_matrix
        steps_to_exit =  y_length `div` slope_y

        x_indices = map (*slope_x) [1..steps_to_exit]
        x_indices_pbc = map (+1) $ map ( `mod` (x_length)) x_indices

        y_indices = map (+1) $ map (*slope_y) [1..steps_to_exit-1]

        indices = zip  y_indices x_indices_pbc
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

    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    let trees = map (uncurry $ find_trees map_array) slopes

    print trees
    print $ product trees

    hClose handle