import Aux

-------------------------------------------------------
-- a) Normalize Polynomial
-------------------------------------------------------

-- Normalize Polynomial
perfectPolynomial :: Polynomial -> Polynomial
perfectPolynomial [] = []
perfectPolynomial poly = (clearPolynomial (normalizePolynomial poly))

-- Sums the coefficient of monomials with the same variables and degrees
-- Clears monomials with coefficient 0
normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial [] = []
normalizePolynomial (mono:poly) = [Mono (sumCoefficients (mono:(filteredMonomials mono poly))) (variables mono)] ++ normalizePolynomial (clearUsedMonomials mono poly)   

-- Filters monomials with the same variables as 'mono'
filteredMonomials :: Monomial -> Polynomial -> Polynomial
filteredMonomials mono poly = [eq_mono | eq_mono <- poly, (sort (variables mono)) == (sort (variables eq_mono))]

-- Sums the coefficients from the filtered polynomials
sumCoefficients :: Polynomial -> Int
sumCoefficients poly = sum [coefficient mono | mono <- poly]

-- Removes monomials with the same variables as 'mono'
clearUsedMonomials :: Monomial -> Polynomial -> Polynomial
clearUsedMonomials mono poly = [eq_mono | eq_mono <- poly, (sort (variables mono)) /= (sort (variables eq_mono))]

-- Goes through each monomial and Sums the degree of equal variables
-- Removes variables with degree = 0
-- Removes monomials with coefficient = 0
clearPolynomial :: Polynomial -> Polynomial
clearPolynomial [] = []
clearPolynomial poly = [Mono (coefficient mono) (filterVariables (variables mono)) | mono <-poly, coefficient mono /= 0]

-- Removes variables with degree = 0
filterVariables :: [Variable] -> [Variable]
filterVariables [] = []
filterVariables vars = [var | var <- (joinVariables vars), (degree var) /= 0]

-- Joins variables with the same character and sums the degree
joinVariables :: [Variable] -> [Variable]
joinVariables [] = []
joinVariables (var:vars) = [sumDegree(filterVar var (var:vars)) ] ++ joinVariables (clearVar var (var:vars))

-- Sums the degree of the same variable
sumDegree :: [Variable] -> Variable
sumDegree vars = Var (variable (head vars)) (sum[degree var | var <- vars])

-- Returns an array of variables with the same character as 'var'
filterVar :: Variable -> [Variable] -> [Variable]
filterVar var vars = [v | v <-vars, (variable var) == (variable v)]

-- Returns an array of variables with a different character as 'var'
clearVar :: Variable -> [Variable] -> [Variable]
clearVar var vars = [v | v <-vars, (variable var) /= (variable v)]

-------------------------------------------------------
-- b) Sum Polynomials
-------------------------------------------------------

-- Adds two polynomials together, returning a string of the result
addPolynomials :: String -> String -> String
addPolynomials [] [] = error ("not a valid polynomial")
addPolynomials str1 str2 = polynomial2String( parseString2Poly (concatenateStrings str1  str2 ))

-- Join both strings together, separating them with the proper signal
concatenateStrings :: String -> String -> String
concatenateStrings [] [] = []
concatenateStrings [] str2 = str2
concatenateStrings str1 [] = str1
concatenateStrings str1 str2
                | head(str2) == '-' = str1 ++ " " ++ str2
                | otherwise = str1 ++ " + " ++ str2
                
-------------------------------------------------------
-- c) Multiply Polynomials
-------------------------------------------------------

-- Multiplies two polynomials, returning a string of the result
multiplyPolynomials :: String -> String -> String
multiplyPolynomials [] [] = []
multiplyPolynomials str1 str2 = polynomial2String (clearPolynomial(distributiveLaw (parseString2Poly str1) (parseString2Poly str2)))

-- Multiplies each term in one polynomial by each term in the other polynomial using the distributive law
distributiveLaw :: Polynomial -> Polynomial -> Polynomial
distributiveLaw [] [] = []
distributiveLaw a b = [ multMonos mono1 mono2 | mono1 <- a, mono2 <- b]

-- Multiplies monomials together
multMonos :: Monomial -> Monomial -> Monomial
multMonos a b = Mono ((coefficient a) * (coefficient b)) (variables a ++ variables b)
 
-------------------------------------------------------
-- d) Derive Polynomials
-------------------------------------------------------

-- Derive Polynomial to a variable 'c'
derive :: Char -> String -> String
derive c poly = polynomial2String(map (calculateDerive c) (filterVariable c (parseString2Poly poly)))


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

-- 
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
