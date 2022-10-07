import Data.List

type Mon = (Int, String, Int) -- monomial where the first member is the number, the second the variable and the third the exponent, ex 7*y^2 = (7,"y",2)
type Poly = [Mon]
--pol ex : [(0,"x",2),(2,"y",0),(5,"z",0),(1,"y",0),(7,"y",2)]

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

