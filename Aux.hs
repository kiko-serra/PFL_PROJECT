module Aux where
import Data.Char
import Data.List

-------------------------------------------------------
-- Data & Types ---------------------------------------
-------------------------------------------------------

data Variable = Var { variable :: Char, degree :: Int} deriving (Show, Eq, Read, Ord)

data Monomial = Mono { coefficient :: Int, variables :: [Variable]} deriving (Show, Eq, Read, Ord)

type Polynomial = [Monomial]

-------------------------------------------------------
-- Parsing string to polynomial
-------------------------------------------------------

-- Main function: Parses string to a polynomial
input :: String -> Polynomial
input str = [string2Monomial x | x <- splitString str]

-- Split candidate monomials into multiple strings when a space is found
splitString :: String -> [String]
splitString [] = [[]]
splitString str = words (addSpace2String (str ++ [' ']))

-- Add space between 2 candidate monomials
addSpace2String :: String -> String
addSpace2String [] = []
addSpace2String (c:str)
                | isDigit c && isLetter (head str) = [c] ++ ['*'] ++  addSpace2String str
                | isLetter c && isLetter (head str) = [c] ++ ['*'] ++ addSpace2String str
                | isLetter c && isDigit (head str) = [c] ++ ['*'] ++ addSpace2String str
                | c == ' ' = addSpace2String str
                | c == '^' && head str == '-' = [c] ++ [head str] ++ addSpace2String (tail str)
                | c == '-' = [' '] ++ [c] ++ addSpace2String str
                | c == '+' = [' '] ++ addSpace2String str
                | otherwise = [c] ++ addSpace2String str

-- Constructs a monomial with the transformed string given
string2Monomial :: String -> Monomial
string2Monomial str
            | length str == 1 && isDigit  (head str) = Mono (digitToInt (head str)) []
            | length str == 1 && isLetter (head str) = Mono 1 [Var (head str) 1]
            | length str == 2 && isDigit  (head (tail str)) && head str == '-' = Mono (negate (digitToInt (head (tail str)))) []
            | length str == 2 && isLetter (head (tail str)) && head str == '-' = Mono (negate 1) [Var (head (tail str)) 1]
            | isDigit (head str) && digitToInt (head str) == 0 = Mono 0 []
            | head str == '-' = Mono (negate (getCoefficient (tail str))) (getVariables (splitVariable (coefficientFreeString (tail str))))
            | otherwise = Mono (getCoefficient str) (getVariables (splitVariable (coefficientFreeString str)))

-- Get coefficient from the string, reading while it finds a digit
-- Otherwise, the coefficient is 1
getCoefficient :: String -> Int
getCoefficient str
            | isDigit (head str) = read (takeWhile isDigit str) :: Int
            | otherwise = 1

-- Removes the coefficient from the string
-- Retrieves variables and degrees
coefficientFreeString :: String -> String
coefficientFreeString [] = []
coefficientFreeString (c:str)
            | isDigit c = coefficientFreeString str
            | c == '*' = coefficientFreeString str
            | otherwise = [c] ++ str

-- Split variables string when a space is found
splitVariable :: String -> [String]
splitVariable [] = [[]]
splitVariable str = words (addSpace2Variable (str ++ [' ']))

-- Add space whenever a '*' is found on the string
addSpace2Variable :: String -> String
addSpace2Variable [] = []
addSpace2Variable (c:str)
                | c == '*' = " " ++ addSpace2Variable str
                | otherwise = [c] ++ addSpace2Variable str

-- Creates an array of variables
-- Calls getVariable function for each string
getVariables :: [String] -> [Variable]
getVariables [] = []
getVariables strs = [getVariable (remExponentSymbol str) | str <- strs]

-- Removes the exponent symbol from the string
remExponentSymbol :: String -> String
remExponentSymbol [] = []
remExponentSymbol (c:str)
        | c == '^' = remExponentSymbol str
        | otherwise = [c] ++ remExponentSymbol str

-- Constructs a Variable with a given character and degree
getVariable :: String -> Variable
getVariable (var:degree)
            | isLetter var && null degree = Var var 1
            | isLetter var && isDigit (head degree) = Var var (read degree :: Int)
            | isLetter var && (head degree) == '-' && isDigit (head (tail degree)) = Var var (read degree :: Int)
            | otherwise = error "Variable degree not valid"

-------------------------------------------------------
-- Polynomial to String
-------------------------------------------------------

-- Transforms Polynomial into a readable String
output :: Polynomial -> String
output [] = []
output poly = auxFuncPoly2String (clearPolynomial (normalizePolynomial poly))


-- Auxiliar function that puts spaces between monomials
-- If the following monomial has a positive coefficient, puts '+' into the string
auxFuncPoly2String :: Polynomial -> String
auxFuncPoly2String [] = []
auxFuncPoly2String (mono:poly)
            | null poly = monomial2String mono
            | coefficient (head poly) >= 0 = monomial2String mono ++ " + " ++ auxFuncPoly2String poly
            | otherwise = monomial2String mono ++ " " ++ auxFuncPoly2String poly

-- Transforms monomials into a readable string
-- If the coefficient is either '1' or '-1', only the signal is showed
monomial2String :: Monomial -> String
monomial2String mono
            | (coefficient mono == 1) && not (null (variables mono)) = noCoeffVar2String (variables mono)
            | (coefficient mono == -1) && not (null (variables mono)) = "-" ++ noCoeffVar2String (variables mono)
            | otherwise = show (coefficient mono) ++ variables2String (variables mono)

-- Transforms variables into a readable string
-- Puts '*' to separate variables and '^' to show the degree
variables2String :: [Variable] -> String
variables2String [] = []
variables2String (var:remainder)
            | degree var == 0 = variables2String remainder
            | degree var == 1 = "*" ++ [variable var] ++ variables2String remainder
            | otherwise = "*" ++ [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder

-- Similar to the variables2String function
-- Doesn't put '*' at the start since there is no coefficient
-- Calls the variables2String function for the remainder variables
noCoeffVar2String :: [Variable] -> String
noCoeffVar2String [] = []
noCoeffVar2String (var:remainder)
            | degree var == 0 = variables2String remainder
            | degree var == 1 = [variable var] ++ variables2String remainder
            | otherwise = [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder


-------------------------------------------------------
-- Normalize auxliars
-------------------------------------------------------

-- Sums the coefficient of monomials with the same variables and degrees
-- Clears monomials with coefficient 0
normalizePolynomial :: Polynomial -> Polynomial
normalizePolynomial [] = []
normalizePolynomial (mono:poly) = clearPolynomial (Mono (sumCoefficients (mono:filteredMonomials mono poly)) (variables mono) : normalizePolynomial (clearUsedMonomials mono poly))

-- Filters monomials with the same variables as 'mono'
filteredMonomials :: Monomial -> Polynomial -> Polynomial
filteredMonomials mono poly = [eq_mono | eq_mono <- poly, sort (variables mono) == sort (variables eq_mono)]

-- Sums the coefficients from the filtered polynomials
sumCoefficients :: Polynomial -> Int
sumCoefficients poly = sum [coefficient mono | mono <- poly]

-- Removes monomials with the same variables as 'mono'
clearUsedMonomials :: Monomial -> Polynomial -> Polynomial
clearUsedMonomials mono poly = [eq_mono | eq_mono <- poly, sort (variables mono) /= sort (variables eq_mono)]

-- Goes through each monomial and Sums the degree of equal variables
-- Removes variables with degree = 0
-- Removes monomials with coefficient = 0
clearPolynomial :: Polynomial -> Polynomial
clearPolynomial [] = []
clearPolynomial poly = [Mono (coefficient mono) (filterVariables (variables mono)) | mono <-poly, coefficient mono /= 0]

-- Removes variables with degree = 0
filterVariables :: [Variable] -> [Variable]
filterVariables [] = []
filterVariables vars = [var | var <- joinVariables vars, degree var /= 0]

-- Joins variables with the same character and sums the degree
joinVariables :: [Variable] -> [Variable]
joinVariables [] = []
joinVariables (var:vars) = sumDegree(filterVar var (var:vars)) : joinVariables (clearVar var (var:vars))

-- Sums the degree of the same variable
sumDegree :: [Variable] -> Variable
sumDegree vars = Var (variable (head vars)) (sum[degree var | var <- vars])

-- Returns an array of variables with the same character as 'var'
filterVar :: Variable -> [Variable] -> [Variable]
filterVar var vars = [v | v <-vars, variable var == variable v]

-- Returns an array of variables with a different character as 'var'
clearVar :: Variable -> [Variable] -> [Variable]
clearVar var vars = [v | v <-vars, variable var /= variable v]