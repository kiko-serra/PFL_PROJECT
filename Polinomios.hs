import Data.Char

--bibliotecas que podem ser usadas
--Data.List Data.String Data.Char

data Monomio = Mult Int | Var Char | Exp Int
-- polinomio do tipo numero * variavel ^ expoente
--normalize polynomials
scanner :: String -> [Monomio]
scanner "" = []
scanner (x : xs)
  | isDigit x = Mult (digitToInt x) : scanner xs
  | isAlpha x = Var x : scanner xs
  | isDigit (head xs) = Exp (digitToInt (head xs)) : scanner (tail xs)
  | otherwise = scanner xs

--objetivo era guardar logo o primeiro monomio e depois ir guardando os restantes
scanner2 :: String -> [Monomio]
scanner2 "" = []
scanner2 (x : y: z : w : t : xs) = Mult (digitToInt x) : Var z : Exp (digitToInt t) : scanner2 xs