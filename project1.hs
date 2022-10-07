import Data.List
import Data.Char

type Mon = (Int, String, Int) -- monomial where the first member is the number, the second the variable and the third the exponent, ex 7*y^2 = (7,"y",2)
type Poly = [Mon]
--pol ex : [(0,"x",2),(2,"y",0),(5,"z",0),(1,"y",0),(7,"y",2)]
-- test of sum: uiSumPol [(3,"x",2),(6,"x",1),(4,"y",2),(5,"x",1)] [(7,"x",1),((-6),"y",2),(4,"x",1)]
              --uiSumPol [(10,"x",2),((-10),"y",2),((-1),"x",1),((-10),"x",2),(5,"~",0)] [(5,"z",6),(10,"y",2),((-19),"~",0),(4,"x",1),((-7),"w",4)]
--By convention, a polynomial like 7 (doesn't have a variable) will be written like (7,"~",0)

getExp :: Mon -> Int
getExp (_,_,x) = x

getVar :: Mon -> String
getVar (_,s,_) = s

getNum :: Mon -> Int
getNum (n,_,_) = n

printMon :: Mon -> IO ()
printMon m = putStr (show (getNum m) ++ "*" ++ (getVar m) ++ (if (getExp m > 1) then ("^" ++ show (getExp m) ++ "\n") else "\n"))

getMonStr :: Mon -> String
getMonStr m = (show (getNum m) ++ "*" ++ (getVar m) ++ (if (getExp m > 1) then ("^" ++ show (getExp m)) else ""))

sumMon :: Mon -> Mon -> Mon
sumMon m1 m2 = (getNum m1 + getNum m2,getVar m1, getExp m1)

normPol :: Poly -> Poly --Normalizes the Polynomial by removing zeros, sorting and adding all of the monomials that can be summed
normPol p = foldl (\acc m -> if (length (filterVarAndExp acc m) == 0) then acc ++ [m] else (reverseFilterVarAndExp acc m) ++ [sumMon m (head (filterVarAndExp acc m))]  ) [] (sortPol (removeZeros p))

sortPol :: Poly -> Poly -- Sorts the Polynomial first by the letter of the variable in alphabaetic order and then by the exponent in descending order
sortPol p = sortBy (\(_,_,a) (_,_,b) -> flip compare a b) (sortBy (\(_,a,_) (_,b,_) -> compare a b) p)


removeZeros :: Poly -> Poly -- Removes monomials containing 0 like 0y^2
removeZeros p = filter (\(a,_,_) -> a /= 0) p


filterVarAndExp :: Poly -> Mon -> Poly --Returns a polynomial that may only contains only the monomials that have the same variable and exponent as the one given
filterVarAndExp p m = filter (\(_,b,c) -> (b == (getVar m)) && (c == (getExp m))) p

reverseFilterVarAndExp :: Poly -> Mon -> Poly --The reverse of function filterVarAndExp, this one returns a Polynomial without any monomials that contain the same variable and exponent as the one given
reverseFilterVarAndExp p m = filter (\(_,b,c) -> (b /= (getVar m)) || (c /= (getExp m))) p

printPol :: Poly -> String
printPol [] = ""
printPol (p:ps) = (getMonStr p)++ " + " ++ (printPol ps)

sumPol :: Poly -> Poly -> Poly --Calculates recursively the sum of two polynomials. uses recursion to try to sum every member of p1 with some member from p2
sumPol p1 [] = p1
sumPol [] p2 = []
sumPol (p1:ps1) p2 = if (length (filterVarAndExp p2 p1) == 0) then [p1] ++ sumPol ps1 p2 else ([sumMon p1 (head (filterVarAndExp p2 p1))]) ++ sumPol ps1 p2

sumPolVariableRecover :: Poly -> Poly -> Poly --Due to the nature of the sumPol function, if there are any variables in p2 that are not in p1 they will not be added to the final Poly and so this function goes recursively through p2 and returns a poly of the missing variables
sumPolVariableRecover [] p2 = p2
sumPolVariableRecover p1 [] = []
sumPolVariableRecover p1 (p2:ps2) = if (existsVar p1 p2) then sumPolVariableRecover p1 ps2 else [p2] ++ sumPolVariableRecover p1 ps2

existsVar :: Poly -> Mon -> Bool --Checks whether or not a variable from a monomial exists in a polynomial
existsVar p m = if (length (filter (\(_,b,_) -> (b == (getVar m))) p) == 0) then False else True    


uiSumPol :: Poly -> Poly -> Poly --Guarentees that the polynomials are normalized before trying to calculate their sum and normalizes the resulting polynomial since some members may become 0, ex: 10*x^2 - 10*x^2
uiSumPol p1 p2 = normPol ((sumPol (normPol p1) (normPol p2)) ++ (sumPolVariableRecover p1 p2))

