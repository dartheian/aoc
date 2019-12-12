import System.Environment

fuel :: Int -> Int
fuel x = if x >= 6 then f + fuel f else 0 where f = x `quot` 3 - 2

main = do
    args    <- getArgs
    content <- readFile (head args)
    let masses     = map read (lines content)
    let total_fuel = foldl (+) 0 (map fuel masses)
    print total_fuel