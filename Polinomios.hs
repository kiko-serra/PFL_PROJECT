import Data.Char   
import Data.List 

-- ADICIONAR O PACKAGE QUICKCHECK
-- import Test.QuickCheck
-- prop :: [Int] -> Bool
-- prop l = reverse (reverse l) == l
-- terminal fazer::: -> quickCheck prop

-- Example: "1 + 0 + x + 2*x + x^2 + 2*x^2 -1 -x -x^-2 + x*y + 0*x*y"

-- Variable 'N' refers to no variable --> 2 = 2*N^0

---- TODO -------->   make "x*x" something possible

-- Data -----------------------------------------------

data Variables = Var { variable :: Char, degree :: Int} deriving (Show, Eq, Read, Ord)

data Monomial = Mono { coefficient :: Int, variables :: [Variables]} deriving (Show, Eq, Read, Ord)

type Polynomial = [Monomial]

-------------------------------------------------------

-- Add spaces to break into multiple strings ----------

addSpace :: String -> String 
addSpace [] = []
addSpace (c:str)
                | isDigit c && isLetter (head str) = [c] ++ ['*'] ++  addSpace str
                | isLetter c && isLetter (head str) = [c] ++ ['*'] ++ addSpace str
                | c == ' ' = addSpace str
                | c == '^' && head(str) == '-' = [c] ++ [head str] ++ addSpace (tail str)
                | c == '-' = [' '] ++ [c] ++ addSpace str
                | c == '+' = [' '] ++ addSpace str
                | otherwise = [c] ++ addSpace str


breakString :: String -> [String]
breakString str = words (addSpace (str ++ [' '])) -- Space added to not fail functions

-------------------------------------------------------

-- Get Coefficient from the string
getCoefficient :: String -> Int
getCoefficient str 
            | isDigit (head str) = read (takeWhile isDigit str) :: Int
            | (head str) == '-' && isDigit (head (tail str)) = read ("-" ++ takeWhile isDigit (tail str)) :: Int
            | otherwise = 1

getDegree :: String -> Int
getDegree str 
            | isDigit (head str) = read (takeWhile isDigit str) :: Int
            | otherwise = error "Degree not valid"

coefficientFreeString :: String -> String
coefficientFreeString (c:str) 
            | isDigit c = coefficientFreeString str
            | c == '*' = coefficientFreeString str
            | otherwise = [c] ++ str


parseForVariable :: String -> String
parseForVariable [] = []
parseForVariable (c:str) 
        | c == '^' = parseForVariable str
        | otherwise = [c] ++ parseForVariable str

getVariable :: String -> Variables
getVariable (var:degree)
            | isLetter var && null degree = Var var 1
            | isLetter var && isDigit (head degree) = Var var (read degree :: Int)
            | isLetter var && (head degree) == '-' && isDigit (head (tail degree)) = Var var (read degree :: Int)
            | otherwise = error "Degree not valid"

getVariables :: [String] -> [Variables]
getVariables [] = []
getVariables (str:strs) = [getVariable (parseForVariable str)] ++ getVariables strs
            
addSpace2Variable :: String -> String
addSpace2Variable [] = []
addSpace2Variable (c:str)
                | c == '*' = " " ++ addSpace2Variable str 
                | otherwise = [c] ++ addSpace2Variable str


splitVariable :: String -> [String]
splitVariable [] = [[]]
splitVariable str = words (addSpace2Variable (str ++ [' ']))

string2Monomial :: String -> Monomial
string2Monomial str
            | length str == 1 && isDigit (head str) = Mono (digitToInt (head str)) []
            | length str == 1 && isLetter (head str) = Mono 1 [Var (head str) 1]
            | length str == 2 && head str == '-' && isDigit (head (tail str)) = Mono (negate (digitToInt (head (tail str)))) []
            | length str == 2 && head str == '-' && isLetter (head (tail str)) = Mono (negate 1) [Var (head (tail str)) 1]
            | isDigit (head str) && digitToInt (head str) == 0 = Mono 0 []
            | head str == '-' = Mono (negate (getCoefficient (tail str))) (getVariables (splitVariable (coefficientFreeString (tail str))))
            | otherwise = Mono (getCoefficient str) (getVariables (splitVariable (coefficientFreeString str)))


auxString2Ponomial :: [String] -> [Monomial]
auxString2Ponomial [] = []
auxString2Ponomial (str:strs) = [string2Monomial str] ++ auxString2Ponomial strs

string2Polynomial :: String -> Polynomial
string2Polynomial str =  clearPolynomial(removeCoeff0( auxString2Ponomial (breakString str)))


-- Clear Polynomial of coefficient 0 and variables with degree 0

removeCoeff0 :: Polynomial -> Polynomial
removeCoeff0 [] = []
removeCoeff0 poly = [mono | mono <- poly, coefficient mono /= 0]


removeExponents :: [Variables] -> [Variables]
removeExponents [] = []
removeExponents vars = [v | v <- vars, (degree v) /= 0]

clearVar :: Variables -> [Variables] -> [Variables]
clearVar var vars = [v | v <-vars, (variable var) /= (variable v)]

filterVar :: Variables -> [Variables] -> [Variables]
filterVar var vars = [v | v <-vars, (variable var) == (variable v)]

sumDeg :: [Variables] -> Variables
sumDeg vars = Var (variable (head vars)) (sum[degree var | var <- vars])

sumDegFromSameExp :: [Variables] -> [Variables]
sumDegFromSameExp [] = []
sumDegFromSameExp (var:vars) = [sumDeg(filterVar var (var:vars)) ] ++ sumDegFromSameExp (clearVar var (var:vars))

filterExponents :: [Variables] -> [Variables]
filterExponents [] = []
filterExponents vars = sumDegFromSameExp(removeExponents (vars))


clearPolynomial :: Polynomial -> Polynomial
clearPolynomial [] = []
clearPolynomial poly = [Mono (coefficient mono) (filterExponents (variables mono)) | mono <-poly]


-- Sort (decreasing) and combine similar monomials

sumCoefficients :: Polynomial -> Int
sumCoefficients poly = sum [coefficient mono | mono <- poly]

equalVariables :: [Variables] -> [Variables] -> Bool
equalVariables slave tester = (sort slave) == (sort tester)

clearUsedMonomials :: Monomial -> Polynomial -> Polynomial
clearUsedMonomials mono poly = [eq_mono | eq_mono <- poly, not (equalVariables (variables mono) (variables eq_mono))]

filteredMonomials :: Monomial -> Polynomial -> Polynomial
filteredMonomials mono poly = [eq_mono | eq_mono <- poly, equalVariables (variables mono) (variables eq_mono)]

auxNormalizePoly :: Polynomial -> Polynomial
auxNormalizePoly [] = []
auxNormalizePoly (mono:poly) = [Mono (sumCoefficients (mono:(filteredMonomials mono poly))) (variables mono)] ++ auxNormalizePoly (clearUsedMonomials mono poly)           

-- Transform Polynomial into a readable one -----------

variables2StringV2 :: [Variables] -> String
variables2StringV2 [] = []
variables2StringV2 (var:remainder) 
            | degree var == 0 = variables2String remainder
            | degree var == 1 = [variable var] ++ variables2String remainder
            | otherwise = [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder
         


variables2String :: [Variables] -> String
variables2String [] = []
variables2String (var:remainder) 
            | degree var == 0 = variables2String remainder
            | degree var == 1 = "*" ++ [variable var] ++ variables2String remainder
            | otherwise = "*" ++ [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder
            

monomial2String :: Monomial -> String
monomial2String mono 
            | (coefficient mono == 1) && not (null (variables mono)) = variables2StringV2 (variables mono)
            | (coefficient mono == -1) && not (null (variables mono)) = "-" ++ variables2StringV2 (variables mono)
            | (coefficient mono > 0) = show (coefficient mono) ++ variables2String (variables mono)
            | otherwise = show (coefficient mono) ++ variables2String (variables mono)

polynomial2String :: Polynomial -> String
polynomial2String [] = []
polynomial2String (mono:poly) 
            | null poly = monomial2String mono
            | coefficient (head poly) >= 0 = monomial2String mono ++ " + " ++ polynomial2String poly
            | otherwise = monomial2String mono ++ " " ++ polynomial2String poly

parsePolynomial2String :: Polynomial -> String
parsePolynomial2String [] = []
parsePolynomial2String poly = polynomial2String (clearPolynomial (removeCoeff0( auxNormalizePoly poly)))

cleanUpPolynomial :: Polynomial -> Polynomial
cleanUpPolynomial [] = []
cleanUpPolynomial poly = clearPolynomial (removeCoeff0( auxNormalizePoly poly))

-------------------------------------------------------

--- a) normalize poly ---------------------------------

normalizePolynomials :: String -> String
normalizePolynomials [] = []
normalizePolynomials str = parsePolynomial2String(string2Polynomial str)

--- b) add polynomials --------------------------------

concatenateStrings :: String -> String -> String
concatenateStrings [] [] = []
concatenateStrings str1 str2
                | head(str2) == '-' = str1 ++ " " ++ str2
                | otherwise = str1 ++ " + " ++ str2

addPolynomials :: String -> String -> String
addPolynomials [] [] = error ("not a valid polynomial")
addPolynomials str1 str2 = parsePolynomial2String( string2Polynomial (concatenateStrings str1  str2 ))

--- c) multiply polynomials ----------------------------

multMonos :: Monomial -> Monomial -> Monomial
multMonos a b 
        | null(variables a) && null(variables b) = Mono ((coefficient a) * (coefficient b)) []
        | null(variables a) = Mono ((coefficient a) * (coefficient b)) (variables b)
        | null(variables b) = Mono ((coefficient a) * (coefficient b)) (variables a)
        | otherwise = Mono ((coefficient a) * (coefficient b)) (variables a ++ variables b)

tryOut :: [Char] -> [Char] -> [(Char, Char)]
tryOut a b = [(x,y) | x <- a, y <- b]

auxMultPoly :: Polynomial -> Polynomial -> Polynomial
auxMultPoly [] [] = []
auxMultPoly a b = [ multMonos mono1 mono2 | mono1 <- a, mono2 <- b]

multiplyPolynomials :: String -> String -> String
multiplyPolynomials [] [] = []
multiplyPolynomials str1 str2 = parsePolynomial2String (cleanUpPolynomial(auxMultPoly (cleanUpPolynomial(string2Polynomial str1)) (cleanUpPolynomial(string2Polynomial str2))))



--- d) derivate poly -----------------------------------
filterWithVar :: Char -> Polynomial -> Polynomial
filterWithVar n [] = []
filterWithVar n xs =  [mono | mono <- xs, checkIfVarEq n (variables mono)]


checkIfVarEq :: Char -> [Variables] -> Bool
checkIfVarEq n [] = False
checkIfVarEq n (v:var)
                | variable v == n = True
                | otherwise = checkIfVarEq n var


--filterWithVar returns the polynimial only with derivable monomials

derive :: Char -> String -> String
derive n poly = parsePolynomial2String(map (calculateDerive n) (filterWithVar n (string2Polynomial poly)))

calculateDerive :: Char -> Monomial -> Monomial
calculateDerive n mono = head [Mono (coefficient mono * degree var) (degreeDown n (variables mono)) | var <- variables mono, variable var == n]

degreeDown :: Char -> [Variables] -> [Variables]
degreeDown n [] = []
degreeDown n (v:var)
                | n /= variable v = v:degreeDown n var
                | otherwise = Var (variable v) (degree v - 1):var



-------------------------------------------------------

main = do   
        putStrLn "Insert the polynomial: "
        asked <- getLine
        putStrLn ("Result: " ++ polynomial2String (string2Polynomial asked))

