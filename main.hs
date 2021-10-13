-- as the parsing is done by words, the expression is only valid
-- when the oporations and operands are separated by spaces
main :: IO ()
main = do
    putStrLn "Expression: "
    exp <- getLine
    putStrLn . show . arithmetic . toTriple . words $ exp

-- perform arithmetic given a String triple of the form
-- (X, operation, Y)
arithmetic :: (String, String, String) -> Double
arithmetic (a, op, b)
    | op == "+" = x + y
    | op == "-" = x - y
    | op == "*" = x * y
    | op == "/" = x / y
    | otherwise = error "Unsupported operation"
    where x = read a :: Double
          y = read b :: Double

-- convert String list to String triple
toTriple :: [String] -> (String, String, String)
toTriple (x:y:z:_) = (x, y, z)

