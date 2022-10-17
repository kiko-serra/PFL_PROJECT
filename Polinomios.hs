import Data.Char   

data Variables = Var { variable :: Char, degree :: Int} deriving (Show, Eq, Read)

data Monomial = Mono { coefficient :: Int, variables :: [Variables]} deriving (Show, Eq, Read)

data Polynomial = Poly { monomials :: [Monomial]} deriving (Show, Eq, Read)

-- Split into separate Monomials -------

_addSpaceToBreak :: String -> String 
_addSpaceToBreak [] = []
_addSpaceToBreak (c:str)
                | isDigit c && isLetter (head str) = [c] ++ ['*'] ++  _addSpaceToBreak str
                | isLetter c && isLetter (head str) = [c] ++ ['*'] ++ _addSpaceToBreak str
                | c == ' ' = _addSpaceToBreak str
                | c == '^' && head(str) == '-' = [c] ++ [head str] ++ _addSpaceToBreak (tail str)
                | c == '-' = [' '] ++ [c] ++ _addSpaceToBreak str
                | c == '+' = [' '] ++ _addSpaceToBreak str 
                | otherwise = [c] ++ _addSpaceToBreak str


_breakString :: String -> [String]
_breakString str = words (_addSpaceToBreak (str ++ [' '])) -- Space added to not fail functions


-- Split into separate Monomials -------

-- n
-- x
-- 2*x
-- x^2
getCoefficient :: String -> Int
getCoefficient str 
            | isDigit (head str) = read (takeWhile isDigit str) :: Int
            | otherwise = 1

getDegree :: String -> Int
getDegree str 
            | isDigit (head str) = read (takeWhile isDigit str) :: Int
            | otherwise = error "Degree not valid"

coefficientFreeString :: String -> String
-- coefficientFreeString str = dropWhile isDigit str
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
            | otherwise = error "Degree not valid"

getVariables :: [String] -> [Variables]
getVariables [] = []
getVariables (str:strs) = [getVariable (parseForVariable str)] ++ getVariables strs
            

string2Monomial :: String -> Monomial
string2Monomial str
            | length str == 1 && isDigit (head str) = Mono (digitToInt (head str)) [Var 'N' 0]
            | length str == 1 && isLetter (head str) = Mono 1 [Var (head str) 1]
            | otherwise = Mono (getCoefficient str) (getVariables (_breakString (coefficientFreeString str)))


auxString2Monomial :: [String] -> [Monomial]
auxString2Monomial [] = []
auxString2Monomial (str:strs) = [string2Monomial str] ++ auxString2Monomial strs

string2Polynomial :: String -> Polynomial
string2Polynomial str = Poly (auxString2Monomial (_breakString str))
            


-- Example: "1 + x^2 + y + 2*y"
