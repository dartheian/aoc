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

read_vectors :: String -> [Vector]
read_vectors s = map read_vector [s' | s' <- split (==',') s]

move :: Point -> Vector -> Point
move (x, y) (direction, magnitude)
    | direction == 'U' = (x, y + magnitude)
    | direction == 'D' = (x, y - magnitude)
    | direction == 'L' = (x - magnitude, y)
    | direction == 'R' = (x + magnitude, y)

generate_points :: String -> [Point]
generate_points s = scanl move (0, 0) [v | v <- read_vectors s]

order_points :: (Point, Point) -> (Point, Point)
order_points ((x1, y1), (x2, y2)) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

generate_segments :: [Point] -> [(Point, Point)]
generate_segments p = [order_points p' | p' <- zip p $ tail p]

horizontal :: (Point, Point) -> Bool
horizontal ((x1, y1), (x2, y2)) = y1 == y2

between :: Int -> Int -> Int -> Bool
between a b c = b <= a && a <= c

manhattan_distance :: Point -> Int
manhattan_distance (x, y) = abs x + abs y

intersection :: (Point, Point) -> (Point, Point) -> Int
intersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    | between x3 x1 x2 && between y1 y3 y4 = manhattan_distance (x3, y1)
    | otherwise = 0

intersect :: (Point, Point) -> (Point, Point) -> Int
intersect s1 s2
    | horizontal s1 && not (horizontal s2) = intersection s1 s2
    | horizontal s2 && not (horizontal s1) = intersection s2 s1
    | otherwise = 0

main = do
    args    <- getArgs
    content <- readFile (head args)
    let segments  = [generate_segments . generate_points $ line | line <- lines content]
    let distances = [intersect s1 s2 | s1 <- segments !! 0, s2 <- segments !! 1]
    print $ minimum $ filter (/=0) distances