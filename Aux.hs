module Aux where
import Data.Char
import Data.List

-- Data definition -----------------------------------------------

data Variables = Var { variable :: Char, degree :: Int} deriving (Show, Eq, Read, Ord)

data Monomial = Mono { coefficient :: Int, variables :: [Variables]} deriving (Show, Eq, Read, Ord)

type Polynomial = [Monomial]

-- Input / Output functions -------------------------------------

--Converts a string into a polynomial
input :: String -> Polynomial
input str =  clearPolynomial(removeCoeff0(auxString2Ponomial (breakString str)))

--Converts a polynomial into a string
output :: Polynomial -> String
output [] = []
output poly = polynomial2String (clearPolynomial (removeCoeff0(auxNormalizePoly poly)))

--Auxiliary Input / Output functions -------------------------------------------

--Input Auxiliary functions

-- Space added to not fail functions
breakString :: String -> [String]
breakString str = words (addSpace (str ++ [' ']))

-- Add spaces to break into multiple strings ----------
addSpace :: String -> String
addSpace [] = []
addSpace (c:str)
                | isDigit c && isLetter (head str) = [c] ++ ['*'] ++  addSpace str
                | isLetter c && isLetter (head str) = [c] ++ ['*'] ++ addSpace str
                | c == ' ' = addSpace str
                | c == '^' && head str == '-' = [c] ++ [head str] ++ addSpace (tail str)
                | c == '-' = [' '] ++ [c] ++ addSpace str
                | c == '+' = [' '] ++ addSpace str
                | otherwise = [c] ++ addSpace str



--DESCRIPTION NEEDS TO BE ADDED
auxString2Ponomial :: [String] -> [Monomial]
auxString2Ponomial [] = []
auxString2Ponomial (str:strs) = [string2Monomial str] ++ auxString2Ponomial strs

--DESCRIPTION NEEDS TO BE ADDED
string2Monomial :: String -> Monomial
string2Monomial str
            | length str == 1 && isDigit (head str) = Mono (digitToInt (head str)) []
            | length str == 1 && isLetter (head str) = Mono 1 [Var (head str) 1]
            | length str == 2 && head str == '-' && isDigit (head (tail str)) = Mono (negate (digitToInt (head (tail str)))) []
            | length str == 2 && head str == '-' && isLetter (head (tail str)) = Mono (negate 1) [Var (head (tail str)) 1]
            | isDigit (head str) && digitToInt (head str) == 0 = Mono 0 []
            | head str == '-' = Mono (negate (getCoefficient (tail str))) (getVariables (splitVariable (coefficientFreeString (tail str))))
            | otherwise = Mono (getCoefficient str) (getVariables (splitVariable (coefficientFreeString str)))

-- Get Coefficient from the string
getCoefficient :: String -> Int
getCoefficient str
            | isDigit (head str) = read (takeWhile isDigit str) :: Int
            | head str == '-' && isDigit (head (tail str)) = read ("-" ++ takeWhile isDigit (tail str)) :: Int
            | otherwise = 1

-- Get Variables from the string
getVariables :: [String] -> [Variables]
getVariables [] = []
getVariables (str:strs) = [getVariable (parseForVariable str)] ++ getVariables strs

-- DESCRIPTION NEEDS TO BE ADDED
parseForVariable :: String -> String
parseForVariable [] = []
parseForVariable (c:str)
        | c == '^' = parseForVariable str
        | otherwise = [c] ++ parseForVariable str

-- DESCRIPTION NEEDS TO BE ADDED
getVariable :: String -> Variables
getVariable (var:degree)
            | isLetter var && null degree = Var var 1
            | isLetter var && isDigit (head degree) = Var var (read degree :: Int)
            | isLetter var && (head degree) == '-' && isDigit (head (tail degree)) = Var var (read degree :: Int)
            | otherwise = error "Degree not valid"

-- DESCRIPTION NEEDS TO BE ADDED
splitVariable :: String -> [String]
splitVariable [] = [[]]
splitVariable str = words (addSpace2Variable (str ++ [' ']))

-- DESCRIPTION NEEDS TO BE ADDED
addSpace2Variable :: String -> String
addSpace2Variable [] = []
addSpace2Variable (c:str)
                | c == '*' = " " ++ addSpace2Variable str
                | otherwise = [c] ++ addSpace2Variable str

-- DESCRIPTION NEEDS TO BE ADDED
coefficientFreeString :: String -> String
coefficientFreeString (c:str)
            | isDigit c = coefficientFreeString str
            | c == '*' = coefficientFreeString str
            | otherwise = [c] ++ str

-- Clear Polynomial of coefficient 0 and variables with degree 0
removeCoeff0 :: Polynomial -> Polynomial
removeCoeff0 [] = []
removeCoeff0 poly = [mono | mono <- poly, coefficient mono /= 0]

--DESCRIPTION NEEDS TO BE ADDED
clearPolynomial :: Polynomial -> Polynomial
clearPolynomial [] = []
clearPolynomial poly = [Mono (coefficient mono) (filterExponents (variables mono)) | mono <-poly]

--DESCRIPTION NEEDS TO BE ADDED
filterExponents :: [Variables] -> [Variables]
filterExponents [] = []
filterExponents vars = sumDegFromSameExp(removeExponents vars)

--DESCRIPTION NEEDS TO BE ADDED
removeExponents :: [Variables] -> [Variables]
removeExponents [] = []
removeExponents vars = [v | v <- vars, degree v /= 0]

--DESCRIPTION NEEDS TO BE ADDED
sumDegFromSameExp :: [Variables] -> [Variables]
sumDegFromSameExp [] = []
sumDegFromSameExp (var:vars) = [sumDeg(filterVar var (var:vars)) ] ++ sumDegFromSameExp (clearVar var (var:vars))

--DESCRIPTION NEEDS TO BE ADDED
clearVar :: Variables -> [Variables] -> [Variables]
clearVar var vars = [v | v <-vars, variable var /= variable v]


--DESCRIPTION NEEDS TO BE ADDED
filterVar :: Variables -> [Variables] -> [Variables]
filterVar var vars = [v | v <-vars, variable var == variable v]

--DESCRIPTION NEEDS TO BE ADDED
sumDeg :: [Variables] -> Variables
sumDeg vars = Var (variable (head vars)) (sum[degree var | var <- vars])

--Output Auxiliary functions

--DESCRIPTION NEEDS TO BE ADDED
auxNormalizePoly :: Polynomial -> Polynomial
auxNormalizePoly [] = []
auxNormalizePoly (mono:poly) = [Mono (sumCoefficients (mono:(filteredMonomials mono poly))) (variables mono)] ++ auxNormalizePoly (clearUsedMonomials mono poly)

--DESCRIPTION NEEDS TO BE ADDED
clearUsedMonomials :: Monomial -> Polynomial -> Polynomial
clearUsedMonomials mono poly = [eq_mono | eq_mono <- poly, not (equalVariables (variables mono) (variables eq_mono))]

--DESCRIPTION NEEDS TO BE ADDED
equalVariables :: [Variables] -> [Variables] -> Bool
equalVariables slave tester = (sort slave) == (sort tester)

--DESCRIPTION NEEDS TO BE ADDED
filteredMonomials :: Monomial -> Polynomial -> Polynomial
filteredMonomials mono poly = [eq_mono | eq_mono <- poly, equalVariables (variables mono) (variables eq_mono)]

--DESCRIPTION NEEDS TO BE ADDED
sumCoefficients :: Polynomial -> Int
sumCoefficients poly = sum [coefficient mono | mono <- poly]

--DESCRIPTION NEEDS TO BE ADDED
polynomial2String :: Polynomial -> String
polynomial2String [] = []
polynomial2String (mono:poly)
            | null poly = monomial2String mono
            | coefficient (head poly) >= 0 = monomial2String mono ++ " + " ++ polynomial2String poly
            | otherwise = monomial2String mono ++ " " ++ polynomial2String poly

--DESCRIPTION NEEDS TO BE ADDED
monomial2String :: Monomial -> String
monomial2String mono
            | (coefficient mono == 1) && not (null (variables mono)) = variables2StringV2 (variables mono)
            | (coefficient mono == -1) && not (null (variables mono)) = "-" ++ variables2StringV2 (variables mono)
            | (coefficient mono > 0) = show (coefficient mono) ++ variables2String (variables mono)
            | otherwise = show (coefficient mono) ++ variables2String (variables mono)

--DESCRIPTION NEEDS TO BE ADDED
variables2StringV2 :: [Variables] -> String
variables2StringV2 [] = []
variables2StringV2 (var:remainder)
            | degree var == 0 = variables2String remainder
            | degree var == 1 = [variable var] ++ variables2String remainder
            | otherwise = [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder

--DESCRIPTION NEEDS TO BE ADDED
variables2String :: [Variables] -> String
variables2String [] = []
variables2String (var:remainder)
            | degree var == 0 = variables2String remainder
            | degree var == 1 = "*" ++ [variable var] ++ variables2String remainder
            | otherwise = "*" ++ [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder

--DESCRIPTION NEEDS TO BE ADDED
cleanUpPolynomial :: Polynomial -> Polynomial
cleanUpPolynomial [] = []
cleanUpPolynomial poly = clearPolynomial (removeCoeff0( auxNormalizePoly poly))