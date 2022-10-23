import Aux

-- ADICIONAR O PACKAGE QUICKCHECK
-- import Test.QuickCheck
-- prop :: [Int] -> Bool
-- prop l = reverse (reverse l) == l
-- terminal fazer::: -> quickCheck prop

-- Example: "1 + 0 + x + 2*x + x^2 + 2*x^2 -1 -x -x^-2 + x*y + 0*x*y"

-- Variable 'N' refers to no variable --> 2 = 2*N^0

---- TODO -------->   make "x*x" something possible

--- a) normalize poly ---------------------------------
normalizePolynomials :: String -> String
normalizePolynomials [] = []
normalizePolynomials str = output(input str)

--- b) add polynomials --------------------------------
addPolynomials :: String -> String -> String
addPolynomials [] [] = error "not a valid polynomial"
addPolynomials str1 str2 = output( input (concatenateStrings str1  str2 ))

concatenateStrings :: String -> String -> String
concatenateStrings [] [] = []
concatenateStrings str1 str2
                | head str2 == '-' = str1 ++ " " ++ str2
                | otherwise = str1 ++ " + " ++ str2

--- c) multiply polynomials ----------------------------
multiplyPolynomials :: String -> String -> String
multiplyPolynomials [] [] = []
multiplyPolynomials str1 str2 = output (cleanUpPolynomial(auxMultPoly (cleanUpPolynomial(input str1)) (cleanUpPolynomial(input str2))))

multMonos :: Monomial -> Monomial -> Monomial
multMonos a b
        | null(variables a) && null(variables b) = Mono (coefficient a * coefficient b) []
        | null(variables a) = Mono (coefficient a * coefficient b) (variables b)
        | null(variables b) = Mono (coefficient a * coefficient b) (variables a)
        | otherwise = Mono (coefficient a * coefficient b) (variables a ++ variables b)

tryOut :: [Char] -> [Char] -> [(Char, Char)]
tryOut a b = [(x,y) | x <- a, y <- b]

auxMultPoly :: Polynomial -> Polynomial -> Polynomial
auxMultPoly [] [] = []
auxMultPoly a b = [ multMonos mono1 mono2 | mono1 <- a, mono2 <- b]

--- d) derivate poly -----------------------------------
derivePolynomial :: Char -> String -> String
derivePolynomial n poly = output(map (calculateDerive n) (filterWithVar n (input poly)))

--Returns the polynimial only with derivable monomials
filterWithVar :: Char -> Polynomial -> Polynomial
filterWithVar n [] = []
filterWithVar n xs =  [mono | mono <- xs, checkIfVarEq n (variables mono)]

--Aux to filterWithVar to check if any variable in a monomial is the same as the one we want to derive
checkIfVarEq :: Char -> [Variables] -> Bool
checkIfVarEq n [] = False
checkIfVarEq n (v:var)
                | variable v == n = True
                | otherwise = checkIfVarEq n var

--Calculates the derivative of a monomial
calculateDerive :: Char -> Monomial -> Monomial
calculateDerive n mono = head [Mono (coefficient mono * degree var) (degreeDown n (variables mono)) | var <- variables mono, variable var == n]

--Aux to calculateDerive to decrease the degree of a variable
degreeDown :: Char -> [Variables] -> [Variables]
degreeDown n [] = []
degreeDown n (v:var)
                | n /= variable v = v:degreeDown n var
                | otherwise = Var (variable v) (degree v - 1):var

-------------------------------------------------------

main = do
        putStrLn "Insert the polynomial: "
        asked <- getLine
        putStrLn ("Result: " ++ polynomial2String (input asked))

