import System.Environment

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

-- read_vector :: String -> Vector
-- read_vector (direction : magnitude) = (direction, read magnitude ::Int)
-- read_vector [] = error "Invalid input"

-- read_vectors :: String -> [Vector]
-- read_vectors s = map read_vector [s' | s' <- split (==',') s]

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile $ head args
    -- let projections = [project_segments . generate_segments $ line | line <- lines content]
    -- let intersections = find_intersections (projections !! 0) (projections !! 1)
    -- let distances = map projection_distance intersections
    -- print $ minimum $ filter (/=0) distances