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
addPolynomials str1 str2 = output(clearPolynomial (perfectPolynomial (input (concatenateStrings str1  str2 ))))

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
multiplyPolynomials str1 str2 = output(clearPolynomial (perfectPolynomial(sortByVariable(clearPolynomial(distributiveLaw (input str1) (input str2))))))

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
derivePolynomial :: Char -> String -> String
derivePolynomial c poly = output(clearPolynomial (perfectPolynomial(map (calculateDerive c) (filterVariable c (input poly)))))


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
-- Test cases
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
testAdd4 = addPolynomials "x+6xyz" "y + 5zyx + 1yzx" == "12*x*y*z + x + y"



testMultiply1 :: Bool
testMultiply1 = multiplyPolynomials "xy" "zy" == "y^2*x*z"

testMultiply2 :: Bool
testMultiply2 = multiplyPolynomials "a+b+c" "d+e-1" == "a*d + a*e -a + b*d + b*e -b + c*d + c*e -c"

testMultiply3 :: Bool
testMultiply3 = multiplyPolynomials "x + x*y" "z+6" == "x*y*z + 6*x*y + x*z + 6*x"

testMultiply4 :: Bool
testMultiply4 = multiplyPolynomials "2x+4a" "3xa^2-5" == "12*a^3*x + 6*x^2*a^2 -20*a -10*x"



testDerive1 :: Bool
testDerive1 = derivePolynomial 'x' "5x^6 +3x^4 -8x^2 + 1" == "30*x^5 + 12*x^3 -16*x"

testDerive2 :: Bool
testDerive2 = derivePolynomial 'x' "x^-5  -2x^-2y + y^2+5" == "4*y*x^-3 -5*x^-6"

testDerive3 :: Bool
testDerive3 = derivePolynomial 'x' (multiplyPolynomials "x + y + y^2*z" "y + x*w") == "y^2*z*w + 2*x*w + y*w + y"

testDerive4 :: Bool
testDerive4 = derivePolynomial 'x' (multiplyPolynomials "x+3 -x*y^-2" "x-3*4x^5") == "18*x^5*y^-2 -18*x^5 -45*x^4 -2*x*y^-2 + 2*x + 3"
