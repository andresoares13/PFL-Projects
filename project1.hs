import Data.List
import Data.Char


--Definition of Polynomial and Monomial

type Mon = (Int, String, Int) -- monomial where the first member is the number, the second the variable and the third the exponent, ex 7*y^2 = (7,"y",2)
type Poly = [Mon] -- polynomial which is a list of one or more monomials    
--By convention, a polynomial like 7 (doesn't have a variable) will be written like (7,"~",0)

--Basic Monomials operations / getters Section

getExp :: Mon -> Int --Returns the value of the exponent
getExp (_,_,x) = x

getVar :: Mon -> String --Returns the string of the variable
getVar (_,s,_) = s

getNum :: Mon -> Int --Returns the number of the monomial, ex: getNum 7*y^2 would return 7
getNum (n,_,_) = n

getMonStr :: Mon -> String --Returns the string of the monomial in the required format, ex: getMonStr (7,"x",2) would return 7*x^2
getMonStr m = (show (abs (getNum m)) ++ (if (getVar m /= "~") then ("*" ++ (getVar m)) else ("")) ++ (if (getExp m > 1) then ("^" ++ show (getExp m)) else ""))

sumMon :: Mon -> Mon -> Mon --Sums two monomials, this function is only called when both monomials in the input have the same variable and exponent
sumMon m1 m2 = (getNum m1 + getNum m2,getVar m1, getExp m1)


--Normalization Section -Task a)

normPol :: Poly -> Poly --Normalizes the Polynomial by removing zeros, sorting and adding all of the monomials that can be summed
normPol p = foldl (\acc m -> if (length (filterVarAndExp acc m) == 0) then acc ++ [m] else (reverseFilterVarAndExp acc m) ++ [sumMon m (head (filterVarAndExp acc m))]  ) [] (sortPol (removeZeros p))

sortPol :: Poly -> Poly -- Sorts the Polynomial first by the letter of the variable in alphabaetic order and then by the exponent in descending order
sortPol p = sortBy (\(_,_,a) (_,_,b) -> flip compare a b) (sortBy (\(_,a,_) (_,b,_) -> compare a b) p)


removeZeros :: Poly -> Poly -- Removes monomials containing 0 like 0y^2
removeZeros p = filter (\(a,_,_) -> a /= 0) p

uiNormPol :: Poly -> IO ()
uiNormPol p = printPolIO (if (getNum (head (normPol p)) > 0) then (getStrPol (normPol p)) else "-" ++ getStrPol (normPol p))



--Helper functions section that filter in and out given variables so that we have polynomials with only those and without those vars


filterVarAndExp :: Poly -> Mon -> Poly --Returns a polynomial that may only contains only the monomials that have the same variable and exponent as the one given
filterVarAndExp p m = filter (\(_,b,c) -> (b == (getVar m)) && (c == (getExp m))) p

reverseFilterVarAndExp :: Poly -> Mon -> Poly --The reverse of function filterVarAndExp, this one returns a Polynomial without any monomials that contain the same variable and exponent as the one given
reverseFilterVarAndExp p m = filter (\(_,b,c) -> (b /= (getVar m)) || (c /= (getExp m))) p



--Print/Get polynomial strings (for the output) Section

getStrPol :: Poly -> String --returns a string of the poly by using a helper function that returns a string of a given monomial, then calculates whether the number is negative or positive and also if it is the last number of the polynomial
getStrPol [] = ""
getStrPol (p:ps) = (getMonStr p) ++ (if (length ps > 0) then (if (getNum (head ps) > 0) then " + " else " - ") else "") ++ (getStrPol ps)

printPolIO :: String -> IO () --Prints the given string
printPolIO s = putStr (s ++ "\n")



--Sum Section -Task b)

sumPol :: Poly -> Poly -> Poly --Calculates recursively the sum of two polynomials. uses recursion to try to sum every member of p1 with some member from p2
sumPol p1 [] = p1
sumPol [] p2 = []
sumPol (p1:ps1) p2 = if (length (filterVarAndExp p2 p1) == 0) then [p1] ++ sumPol ps1 p2 else ([sumMon p1 (head (filterVarAndExp p2 p1))]) ++ sumPol ps1 p2

sumPolVariableRecover :: Poly -> Poly -> Poly --Due to the nature of the sumPol function, if there are any variables in p2 that are not in p1 they will not be added to the final Poly and so this function goes recursively through p2 and returns a poly of the missing variables
sumPolVariableRecover [] p2 = p2
sumPolVariableRecover p1 [] = []
sumPolVariableRecover p1 (p2:ps2) = if (existsVarExp p1 p2) then sumPolVariableRecover p1 ps2 else [p2] ++ sumPolVariableRecover p1 ps2

existsVarExp :: Poly -> Mon -> Bool --Checks whether or not a variable and exponent from a monomial exists in a polynomial, useful when checking if all the monomials that didnt have a match in the sum are in the resulting polynomial
existsVarExp p m = if (length (filter (\(_,b,c) -> (b == (getVar m) && c == (getExp m))) p) == 0) then False else True

sumPolNormalize :: Poly -> Poly -> Poly --Guarentees that the polynomials are normalized before trying to calculate their sum and normalizes the resulting polynomial since some members may become 0, ex: 10*x^2 - 10*x^2
sumPolNormalize p1 p2 = normPol ((sumPol (normPol p1) (normPol p2)) ++ (sumPolVariableRecover p1 p2))


uiSumPol :: Poly -> Poly -> IO () -- Prints the resulting polynomial after both were added and normalized, also given the string output format that we have chosen, we need to confirm whether the first monomial is positive or negative otherwise it will not have the "-" in the string 
uiSumPol p1 p2 = printPolIO (if (getNum (head (sumPolNormalize p1 p2)) > 0) then (getStrPol (sumPolNormalize p1 p2)) else "-" ++ getStrPol (sumPolNormalize p1 p2))


--Multiplication Section -Task c)






--Derivation Section -Task d)

deriveMon :: Mon -> Mon -- Derives one Monomial
deriveMon (a,b,c) = if (c-1 > 0) then (a * c,b,c-1) else (a * c,"~",0)

derivePoly :: Poly -> Poly -- Goes recursively through the polynomial and derives each member (monomial)
derivePoly [] = []
derivePoly (p:ps) = [deriveMon p] ++ derivePoly ps

derivePolyNormalize :: Poly -> Poly --Normalizes the polynomial before and after deriving to make sure everything is in the correct format and cases like 4x + 7 = 4 wont show the 7 since it disappears
derivePolyNormalize p = normPol (derivePoly (normPol p))


uiDerivePol :: Poly -> IO () -- Prints the Polynomial after being derived and normalized, also given the string output format that we have chosen, we need to confirm whether the first monomial is positive or negative otherwise it will not have the "-" in the string
uiDerivePol p = printPolIO (if (getNum (head (derivePolyNormalize p)) > 0) then getStrPol (derivePolyNormalize p) else "-" ++ getStrPol (derivePolyNormalize p))


--Parsing String Input into Our Format Section

stringToInt :: String -> Int -- Converts a String into an Int
stringToInt s = read s :: Int

getNumFromString :: String -> String --Finds recursively the string containing the number from a string of a monomial
getNumFromString "" = ""
getNumFromString (s:sx) = if (s /= '*') then ([s] ++ getNumFromString sx) else ""

getVarFromString :: String -> String -- Finds recursively the variable in a string of a monomial
getVarFromString "" = ""
getVarFromString (s:sx) = if (ord s >= 97 && ord s <= 122) then ([s] ++ getVarFromString sx) else "" ++ getVarFromString sx

getExpFromString :: String -> String -- Finds recursively the exponent in a string of a monomial, only called when the string has a variable
getExpFromString "" = ""
getExpFromString (s:sx) = if (ord s >= 97 && ord s <= 122) then ((filter (\x -> ord x>48 && ord x <=57) sx)) else getExpFromString sx

createMon :: String -> Mon -- Creates a monomial from a string, checks conditions like lack of exponent in the string and a string not containing a variable
createMon s = if (getVarFromString s == "") then (stringToInt (getNumFromString s),"~",0) else (if (getExpFromString s == "") then (stringToInt (getNumFromString s), getVarFromString s,1) else (stringToInt (getNumFromString s), getVarFromString s, stringToInt (getExpFromString s)) )


createPol :: [String] -> Poly
createPol [] = []
createPol (s:sx) = [createMon s] ++ createPol sx

polyParse :: String -> Poly
polyParse s = createPol (words s)



-- User Interface Section

programUI :: IO ()
programUI = do
  putStr "\n\nPolynomial Operations Calculator \n\n\n What would you like to do? \n\n 1. Normalize a polynomial \n 2. Sum two polynomials \n 3. Multiply two polynomials \n 4. Derive a polynomial\n\n"
  option <- getLine
  putStr (option)--temporary



--Tests Section

--test of norm: 
              --uiNormPol[(0,"x",2),(2,"y",0),(5,"z",0),(1,"y",0),(7,"y",2)]
-- test of sum: 
              --uiSumPol [(3,"x",2),(6,"x",1),(4,"y",2),(5,"x",1)] [(7,"x",1),((-6),"y",2),(4,"x",1)]
              --uiSumPol [(7,"x",1),((-6),"y",2),(4,"x",1),(5,"~",0)] [(1,"y",1)]
              --uiSumPol [(10,"x",2),((-10),"y",2),((-1),"x",1),((-10),"x",2),(5,"~",0)] [(5,"z",6),(10,"y",2),((-19),"~",0),(4,"x",1),((-7),"w",4)]
-- test of derivation:
              --uiDerivePol [(7,"x",1),((-6),"y",2),(4,"x",1),(5,"~",0)]          