import Control.Monad

-- main :: IO ()
main = do
    putStrLn "Write the prime to be factored (or ''Exit'' to exit the program):"
    numberToBeFactoredStr <- getLine
    unless (numberToBeFactoredStr == "Exit") $ do
    -- process lines:
    
        putStrLn (numberToBeFactoredStr ++ " = " ++ (show (descomposicionEnPrimos (read numberToBeFactoredStr :: Integer))) ++ ".")

        main

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

-----------------------------------------------------------

divideA :: Integer -> Integer -> Bool
divideA a b = (b `mod` a == 0)

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k
                    | (n `mod` k == 0) = k
                    | otherwise = menorDivisorDesde n (k + 1)

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

descomposicionEnPrimosDesde :: Integer -> Integer -> Integer -> [Integer]
descomposicionEnPrimosDesde n c l
                    | c == l = []
                    | (divideA l n == False) = descomposicionEnPrimosDesde n c (l+1)
                        -- vvv Caso "divideA l n == True": ¿es repetido o no? vvv
                    | ((esPrimo l) && (esPrimo d))      = l : d : []
                    | ((esPrimo l) && not (esPrimo d))  = l :     (descomposicionEnPrimosDesde d ((isqrt(d))+1) (menorDivisor d))
                    | ((esPrimo d) && not (esPrimo l))  =     d : (descomposicionEnPrimosDesde l ((isqrt(l))+1) (menorDivisor l))
                    | otherwise                         =         (descomposicionEnPrimosDesde l ((isqrt(l))+1) (menorDivisor l)) ++ (descomposicionEnPrimosDesde d ((isqrt(d))+1) (menorDivisor d))
                    where d = div n l
                    

descomposicionEnPrimos :: Integer -> [Integer]
descomposicionEnPrimos n
    | esPrimo n = [n]
    | otherwise = descomposicionEnPrimosDesde n ((isqrt (n)) + 1) (menorDivisor n)
