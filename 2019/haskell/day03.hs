import System.Environment

type Vector = (Char, Int)
type Point  = (Int, Int)
type Range  = (Int, Int)

split :: (Char -> Bool) -> String -> [String]
split predicate string =  case dropWhile predicate string of
    "" -> []
    string' -> w : split predicate string''
        where (w, string'') = break predicate string'

parse_vector :: String -> Vector
parse_vector (direction : magnitude) = (direction, read magnitude ::Int)

parse_vectors :: String -> [Vector]
parse_vectors line = map parse_vector [token | token <- split (==',') line]

move :: Point -> Vector -> Point
move (x, y) (direction, magnitude)
    | direction == 'U' = (x, y + magnitude)
    | direction == 'D' = (x, y - magnitude)
    | direction == 'L' = (x - magnitude, y)
    | direction == 'R' = (x + magnitude, y)

generate_curve :: String -> [Point]
generate_curve line = scanl move (0, 0) [vector | vector <- parse_vectors line]

project :: (Point, Point) -> (Range, Range)
project ((x1, y1), (x2, y2)) = ((min x1 x2, max x1 x2), (min y1 y2, max y1 y2))

project_segments :: [Point] -> [(Range, Range)]
project_segments curve = [project segment | segment <- zip curve (tail curve)]

reconstruct_points :: [Int] -> [Int] -> [Point]
reconstruct_points x y =  [(x', y') | x' <- x, y' <- y]

intersect_ranges :: [Int] -> [Int] -> [Int]
intersect_ranges a b
    | a == [] ||b == [] = []
    | head a == head b  = head a : intersect_ranges (tail a) (tail b)
    | head a < head b   = intersect_ranges (tail a) b
    | head a > head b   = intersect_ranges a (tail b)

intersect_projections :: Range -> Range -> [Int]
intersect_projections (a1, b1) (a2, b2) = intersect_ranges [a1..b1] [a2..b2]

intersect_segments :: (Range, Range) -> (Range, Range) -> [Point]
intersect_segments (xa, ya) (xb, yb) = reconstruct_points (intersect_projections xa xb) (intersect_projections ya yb)

find_intersections :: [Point] -> [Point] -> [Point]
find_intersections curve1 curve2 = foldl (++) [] [intersect_segments s1 s2 | s1 <- project_segments curve1, s2 <- project_segments curve2]

manhattan_distance :: Point -> Int
manhattan_distance (x, y) = abs x + abs y

main = do
    args    <- getArgs
    content <- readFile (head args)
    let curves = [generate_curve line | line <- lines content]
    let intersections = find_intersections (curves !! 0) (curves !! 1)
    let distances = map manhattan_distance intersections
    print (minimum (tail distances))