import Data.List

type Mon = (Int, String, Int) -- monomial where the first member is the number, the second the variable and the third the exponent, ex 7*y^2 = (7,"y",2)
type Poly = [Mon]


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

--normPol :: Poly -> Poly
--normPol p = foldl (\acc x -> if ((getVar x) == (getVar acc)) then acc:(sumMon acc x) else acc:x) [(0,"",0)] p

sortPol :: Poly -> Poly
sortPol p = sortBy (\(_,_,a) (_,_,b) -> flip compare a b) (sortBy (\(_,a,_) (_,b,_) -> compare a b) p)
