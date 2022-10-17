import Data.Char   

-- Example: "1 + 0 + x + 2*x + x^2 + 2*x^2 -1 -x -x^-2"

-- Variable 'N' refers to no variable --> 2 = 2*N^0

-- Data -----------------------------------------------

data Variables = Var { variable :: Char, degree :: Int} deriving (Show, Eq, Read)

data Monomial = Mono { coefficient :: Int, variables :: [Variables]} deriving (Show, Eq, Read)

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
            

string2Monomial :: String -> Monomial
string2Monomial str
            | length str == 1 && isDigit (head str) = Mono (digitToInt (head str)) [Var 'N' 0]
            | length str == 1 && isLetter (head str) = Mono 1 [Var (head str) 1]
            | length str == 2 && head str == '-' && isDigit (head (tail str)) = Mono (negate (digitToInt (head (tail str)))) [Var 'N' 0]
            | length str == 2 && head str == '-' && isLetter (head (tail str)) = Mono (negate 1) [Var (head (tail str)) 1]
            | isDigit (head str) && digitToInt (head str) == 0 = Mono 0 []
            | head str == '-' = Mono (negate (getCoefficient (tail str))) (getVariables (breakString (coefficientFreeString (tail str))))
            | otherwise = Mono (getCoefficient str) (getVariables (breakString (coefficientFreeString str)))


auxString2Ponomial :: [String] -> [Monomial]
auxString2Ponomial [] = []
auxString2Ponomial (str:strs) = [string2Monomial str] ++ auxString2Ponomial strs

string2Polynomial :: String -> Polynomial
string2Polynomial str = auxString2Ponomial (breakString str)
            


-- Transform Polynomial into a readable one -----------


variables2String :: [Variables] -> String
variables2String [] = []
variables2String (var:remainder) 
            | degree var == 0 = variables2String remainder
            | otherwise = "*" ++ [variable var] ++ ['^'] ++ show (degree var) ++ variables2String remainder
            

monomial2String :: Monomial -> String
monomial2String mono 
            | (coefficient mono > 0) = "+" ++ show (coefficient mono) ++ variables2String (variables mono)
            | otherwise = show (coefficient mono) ++ variables2String (variables mono)

polynomial2String :: Polynomial -> String
polynomial2String [] = []
polynomial2String (mono:poly) 
            | null poly = monomial2String mono
            | otherwise = monomial2String mono ++ " " ++ polynomial2String poly

-------------------------------------------------------

main = do   
        putStrLn "Insert the polynomial: "
        asked <- getLine
        putStrLn ("Result: " ++ polynomial2String (string2Polynomial asked))

