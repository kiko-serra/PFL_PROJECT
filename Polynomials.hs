import Aux
import Data.List
-------------------------------------------------------
-- a) Normalize Polynomial
-------------------------------------------------------



-- Normalize Polynomial
normalizePolynomial :: String -> String
normalizePolynomial [] = []
normalizePolynomial poly = output (clearPolynomial (perfectPolynomial (input poly)))

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

testNormalize1 :: Bool
testNormalize1 = normalizePolynomial "0+1+2x+3y^2-4xy^3z" == "-4*y^3*x*z + 3*y^2 + 2*x + 1"

testNormalize2 :: Bool
testNormalize2 = normalizePolynomial "-1 +1 +x -x +x^2*x -x^2" == "x^3 -x^2"

testNormalize3 :: Bool
testNormalize3 = normalizePolynomial "x^3*x^-4 + y^3y^-3-1-2" == "x^-1 -2"

testNormalize4 :: Bool
testNormalize4 = normalizePolynomial "x^-3 + x^-2 + 1 + x^-1 + x + x^2" == "x^2 + x + x^-1 + x^-2 + x^-3 + 1"

testAdd1 :: Bool
testAdd1 = addPolynomials "x" "y" == "x + y"

testAdd2 :: Bool
testAdd2 = addPolynomials "a+b+c-2" "+2-4" == "a + b + c -4"

testAdd3 :: Bool
testAdd3 = addPolynomials "x+x-2y^2x" "x+x+y^2*x" == "-y^2*x + 4*x"

testAdd4 :: Bool
testAdd4 = addPolynomials "x+6xyz" "y + 5zyx" == " "
