import Data.Char

data Polinomio = Mult Int | Var Char | Exp Int
-- polinomio do tipo numero * variavel ^ expoente
--normalize polynomials
scanner :: String -> [Polinomio]
scanner "" = []
scanner (x:xs)
            | isDigit x = Mult (digitToInt x) : scanner xs
            | isAlpha x = Var x : scanner xs
            --por condicao para se tiver variavel depois temos que ler o expoente
            | otherwise = scanner xs