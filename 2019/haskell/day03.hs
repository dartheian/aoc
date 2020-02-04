import System.Environment

type Vector = (Char, Int)
type Point  = (Int, Int)
type Range  = (Int, Int)

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
        where (w, s'') = break p s'

read_vector :: String -> Vector
read_vector (direction : magnitude) = (direction, read magnitude ::Int)
read_vector [] = error "Invalid input"

read_vectors :: String -> [Vector]
read_vectors s = map read_vector [s' | s' <- split (==',') s]

move :: Point -> Vector -> Point
move (x, y) (direction, magnitude)
    | direction == 'U' = (x, y + magnitude)
    | direction == 'D' = (x, y - magnitude)
    | direction == 'L' = (x - magnitude, y)
    | direction == 'R' = (x + magnitude, y)
    | otherwise = error "Invalid direction"

generate_segments :: String -> [Point]
generate_segments s = scanl move (0, 0) [v | v <- read_vectors s]

project_segment :: (Point, Point) -> (Range, Range)
project_segment ((x1, y1), (x2, y2)) = ((min x1 x2, max x1 x2), (min y1 y2, max y1 y2))

project_segments :: [Point] -> [(Range, Range)]
project_segments s = [project_segment s' | s' <- zip s $ tail s]

do_ranges_intersect :: Range -> Range -> Bool
do_ranges_intersect (a1, b1) (a2, b2) = not (b1 < a2 || b2 < a1)

do_projections_intersect :: (Range, Range) -> (Range, Range) -> Bool
do_projections_intersect (x1, y1) (x2, y2) = do_ranges_intersect x1 x2 && do_ranges_intersect y1 y2

intersect_ranges :: Range -> Range -> Range
intersect_ranges (a1, b1) (a2, b2) = (max a1 a2, min b1 b2)

intersect_projections :: (Range, Range) -> (Range, Range) -> (Range, Range)
intersect_projections (x1, y1) (x2, y2)
    | do_projections_intersect (x1, y1) (x2, y2) == False = ((0, 0), (0, 0))
    | otherwise = (intersect_ranges x1 x2, intersect_ranges y1 y2)

projection_distance :: (Range, Range) -> Int
projection_distance ((x1, x2), (y1, y2))
    | ((x1, x2), (y1, y2)) == ((0, 0), (0, 0)) = 0
    | x_contains_0 && y_contains_0 = 1
    | x_contains_0 = min (abs y1) (abs y2)
    | y_contains_0 = min (abs x1) (abs x2)
    | otherwise    = min (abs y1) (abs y2) + min (abs x1) (abs x2)
    where x_contains_0 = do_ranges_intersect (x1, x2) (0, 0)
          y_contains_0 = do_ranges_intersect (y1, y2) (0, 0)

find_intersections :: [(Range, Range)] -> [(Range, Range)] -> [(Range, Range)]
find_intersections a b = [intersect_projections p1 p2 | p1 <- a, p2 <- b]

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile $ head args
    let projections = [project_segments . generate_segments $ line | line <- lines content]
    let intersections = find_intersections (projections !! 0) (projections !! 1)
    let distances = map projection_distance intersections
    print $ minimum $ filter (/=0) distances