import Aux
import Data.List
-------------------------------------------------------
-- a) Normalize Polynomial
-------------------------------------------------------

-- Normalize Polynomial
perfectPolynomial :: String -> String
perfectPolynomial [] = []
perfectPolynomial poly = output (clearPolynomial (normalizePolynomial (input poly)))

-------------------------------------------------------
-- b) Sum Polynomials
-------------------------------------------------------

-- Adds two polynomials together, returning a string of the result
addPolynomials :: String -> String -> String
addPolynomials [] [] = error "not a valid polynomial"
addPolynomials str1 str2 = output( input (concatenateStrings str1  str2 ))

-- Join both strings together, separating them with the proper signal
concatenateStrings :: String -> String -> String
concatenateStrings [] [] = []
concatenateStrings [] str2 = str2
concatenateStrings str1 [] = str1
concatenateStrings str1 str2
                | head str2 == '-' = str1 ++ " " ++ str2
                | otherwise = str1 ++ " + " ++ str2

-------------------------------------------------------
-- c) Multiply Polynomials
-------------------------------------------------------

-- Multiplies two polynomials, returning a string of the result
multiplyPolynomials :: String -> String -> String
multiplyPolynomials [] [] = []
multiplyPolynomials str1 str2 = output (clearPolynomial(distributiveLaw (input str1) (input str2)))

-- Multiplies each term in one polynomial by each term in the other polynomial using the distributive law
distributiveLaw :: Polynomial -> Polynomial -> Polynomial
distributiveLaw [] [] = []
distributiveLaw a b = [ multMonos mono1 mono2 | mono1 <- a, mono2 <- b]

-- Multiplies monomials together
multMonos :: Monomial -> Monomial -> Monomial
multMonos a b = Mono (coefficient a * coefficient b) (variables a ++ variables b)

-------------------------------------------------------
-- d) Derive Polynomials
-------------------------------------------------------

-- Derive Polynomial to a variable 'c'
derivePolnomial :: Char -> String -> String
derivePolnomial c poly = output(map (calculateDerive c) (filterVariable c (input poly)))


-- Filters the monomials that have the variable 'c'
filterVariable :: Char -> Polynomial -> Polynomial
filterVariable c [] = []
filterVariable c xs =  [mono | mono <- xs, checkIfVarEq c (variables mono)]

-- Returns true if any variable is equal to 'c', false otherwise
checkIfVarEq :: Char -> [Variable] -> Bool
checkIfVarEq c [] = False
checkIfVarEq c (v:var)
                | variable v == c = True
                | otherwise = checkIfVarEq c var

-- Calculates the derivative of a monomial
calculateDerive :: Char -> Monomial -> Monomial
calculateDerive c mono = head [Mono (coefficient mono * degree var) (degreeDown c (variables mono)) | var <- variables mono, variable var == c]

-- Lowers the degree of the variable 'c'
degreeDown :: Char -> [Variable] -> [Variable]
degreeDown c [] = []
degreeDown c (v:var)
                | c /= variable v = v:degreeDown c var
                | otherwise = Var (variable v) (degree v - 1):var


-------------------------------------------------------
-------------------------------------------------------
-------------------------------------------------------

test1 :: String
test1 = perfectPolynomial "2x^2y +3"