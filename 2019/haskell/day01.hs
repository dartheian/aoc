import System.Environment

fuel' :: Integer -> Integer
fuel' x = x `quot` 3 - 2

fuel :: Integer -> Integer
fuel x | x <= 6    = 0
       | otherwise = f + fuel f
       where f = fuel' x

main :: IO ()
main = do
    args    <- getArgs
    content <- readFile $ head args
    let masses = fmap read $ lines content
    print $ sum $ fmap fuel masses