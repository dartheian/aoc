import System.Environment

fuel :: Integer -> Integer
fuel x = x `quot` 3 - 2

recursiveFuel :: Integer -> Integer
recursiveFuel = sum . takeWhile (> 0) . tail . iterate fuel

totalFuel :: [Integer] -> Integer
totalFuel = sum . (recursiveFuel <$>)

main :: IO ()
main = do
    [filename] <- getArgs
    content    <- readFile filename
    let masses = read <$> lines content
    print $ totalFuel masses