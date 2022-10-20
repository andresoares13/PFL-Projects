module Project1 (programUI,testNormPol,testSumPol,testDerivePol,testMulPol,polyParse) where

import Data.List
import Data.Char


--Definition of Polynomial and Monomial

type Mon = (Int, String, [Int]) -- monomial where the first member is the number, the second the variable and the third the exponent, ex 7*y^2 = (7,"y",2)
type Poly = [Mon] -- polynomial which is a list of one or more monomials    
--By convention, a polynomial like 7 (doesn't have a variable) will be written like (7,"~",0)

--Basic Monomials operations / getters Section

getExp :: Char -> Mon -> Int --Returns the value of the exponent given the varibale
getExp x (_,v,l) = snd (head (filter (\(a,_) -> a == x) (zip v l))) 

getExpL :: Mon -> [Int] --Returns the list of exponents
getExpL (_,_,x) = x

getVar :: Mon -> String --Returns the string of the variable
getVar (_,s,_) = s

getVarIndex :: Char -> Mon -> Int
getVarIndex ch (a,b,c) = snd (head (filter (\(a,b) -> a == ch) (zip b [0..])))

getNum :: Mon -> Int --Returns the number of the monomial, ex: getNum 7*y^2 would return 7
getNum (n,_,_) = n

getVarExpStr :: [(Char,Int)] -> String --Returns a string containing the vars and their exponents, ex: [("x",2)] -> "x^2"
getVarExpStr [] = ""
getVarExpStr ((f,s):ls) = (if (f=='~' || s==0) then "" else [f]++ (if (s>1) then ("^"++ show s) else "")) ++ getVarExpStr ls



getMonStr :: Mon -> String --Returns the string of the monomial in the required format, ex: getMonStr (7,"x",2) would return 7*x^2
getMonStr m = ((if (num == 1) then "" else show (num) )++  getVarExpStr (zip (getVar m) (getExpL m) ))
  where num = abs (getNum m)

sumMon :: Mon -> Mon -> Mon --Sums two monomials, this function is only called when both monomials in the input have the same variable and exponent
sumMon m1 m2 = (getNum m1 + getNum m2,getVar m1, getExpL m1)


--Normalization Section -Task a)

normPol :: Poly -> Poly --Normalizes the Polynomial by removing zeros, sorting and adding all of the monomials that can be summed
normPol p = removeZeros (foldl (\acc m -> if (length (filterVarAndExp acc m) == 0) then acc ++ [m] else (reverseFilterVarAndExp acc m) ++ [sumMon m (head (filterVarAndExp acc m))]  ) [] (sortPolyVars (sortPol (removeZeros p))))

sortPol :: Poly -> Poly -- Sorts the Polynomial first by the letter of the variable in alphabaetic order and then by the exponent in descending order
sortPol p = sortBy (\(_,_,a) (_,_,b) -> flip compare (maximum a) (maximum b)) (sortBy (\(_,a,_) (_,b,_) -> compare a b) p)


removeZeros :: Poly -> Poly -- Removes monomials containing 0 like 0y^2
removeZeros p = filter (\(a,_,_) -> a /= 0) p

uiNormPol :: Poly -> IO () --Prints a given poly in its normalized form
uiNormPol p = if (length nP == 0) then (printPolIO "0") else printPolIO (if (getNum (head (nP)) > 0) then (getStrPol (nP)) else "-" ++ getStrPol (nP))
  where nP = normPol p

testNormPol :: Poly -> String -- To be used in the tests section to be put inside a `putStr` and show the result of the test to the user
testNormPol p = if (length nP == 0) then ("0") else (if (getNum (head (nP)) > 0) then (getStrPol (nP)) else "-" ++ getStrPol (nP))
  where nP = normPol p  



--Helper functions section that filter in and out given variables so that we have polynomials with only those and without those vars


filterVarAndExp :: Poly -> Mon -> Poly --Returns a polynomial that contains only the monomials that have the same variable and exponent as the one given
filterVarAndExp p m = filter (\(_,b,c) -> (b == (sortedVars l)) && (c == (sortedExp l))) p
  where l = getSortedVarExpList (getVar m) (getExpL m)

reverseFilterVarAndExp :: Poly -> Mon -> Poly --The reverse of function filterVarAndExp, this one returns a Polynomial without any monomials that contain the same variable and exponent as the one given
reverseFilterVarAndExp p m = filter (\(_,b,c) -> (b /= (sortedVars l)) || (c /= (sortedExp l))) p
  where l = getSortedVarExpList (getVar m) (getExpL m)

sortedVars :: [(Char,Int)] -> String --receives a list of tuples (var,exponent) and returns the string containing all the variables
sortedVars [] = []
sortedVars (l:ls) = [fst l] ++ sortedVars ls

sortedExp :: [(Char, Int)] -> [Int] --receives a list of tuples (var,exponent) and returns the list containing all the exponents
sortedExp [] = []
sortedExp (l:ls) = [snd l] ++ sortedExp ls

getSortedVarExpList :: String -> [Int] -> [(Char,Int)] -- receives the variables and exponents and returns a list of tuples of pairs of them, sorted by the ascii of the variable
getSortedVarExpList s l = sortBy (\ (a,_) (b,_) -> compare a b) (zip s l)

sortPolyVars :: Poly -> Poly -- Sorts all the vars in the polynomial so that cases like 7xy + 7yx are covered
sortPolyVars [] = []
sortPolyVars (p:ps) = [(getNum p, sortedVars l, sortedExp l)] ++ sortPolyVars ps
  where l = getSortedVarExpList (getVar p) (getExpL p)



--Print/Get polynomial strings (for the output) Section

getStrPol :: Poly -> String --returns a string of the poly by using a helper function that returns a string of a given monomial, then calculates whether the number is negative or positive and also if it is the last number of the polynomial
getStrPol [] = ""
getStrPol (p:ps) = (getMonStr p) ++ (if (length ps > 0) then (if (getNum (head ps) > 0) then " + " else " - ") else "") ++ (getStrPol ps)

printPolIO :: String -> IO () --Prints the given string
printPolIO s = putStr ("\nResult: " ++ s ++ "\n\n")



--Sum Section -Task b)

sumPol :: Poly -> Poly -> Poly --Since the normalization already sums together the monomials, this function appends both polynomials and normalizes them therefore generating their sum
sumPol p1 p2 = normPol (p1 ++ p2)


uiSumPol :: Poly -> Poly -> IO () -- Prints the resulting polynomial after both were added
uiSumPol p1 p2 = if (length sP == 0) then (printPolIO "0") else printPolIO (if (getNum (head (sP)) > 0) then (getStrPol (sP)) else "-" ++ getStrPol (sP))
  where sP = sumPol p1 p2

testSumPol :: Poly -> Poly -> String -- To be used in the tests section to be put inside a `putStr` and show the result of the test to the user
testSumPol p1 p2 = if (length sP == 0) then "0" else (if (getNum (head (sP)) > 0) then (getStrPol (sP)) else "-" ++ getStrPol (sP))
  where sP = sumPol p1 p2  


--Multiplication Section -Task c)

mulMon :: Mon -> Mon -> Mon --Multiplies two monomials by ziping their variables with their correspondent exponents and then unzipping after normalizing said zipped list, also multiplies the number of both monomials
mulMon (a,b,c) (x,y,z) = 
  let mVars = unzipNormVars (normVarsExp (zipVarsExp b y c z))
      mExp = unzipNormExp (normVarsExp (zipVarsExp b y c z))
  in (a*x, mVars, mExp)

zipVarsExp :: String -> String -> [Int] -> [Int] -> [(Char,Int)] --  returns a list of tuples where the first element is a char of the variable and the other is the exponent of that variable
zipVarsExp s1 s2 e1 e2 = sortBy (\(a,_) (b,_) -> compare a b) ((zip s1 e1) ++ (zip s2 e2))

normVarsExp :: [(Char,Int)] -> [(Char,Int)]
normVarsExp l = foldl (\acc (a,b) -> if ((pairFilterVar a acc) == []) then acc ++ [(a,b)] else (reversePairFilterVar a acc) ++ [(a,b+ (snd (head (pairFilterVar a acc))))]) [] l

pairFilterVar :: Char -> [(Char, Int)] -> [(Char, Int)] -- Receives a list of tuples (var,Exp) and a char and returns a list containing only the tuples with that char as their var
pairFilterVar x l = if (length f == 0) then [] else f
  where f = filter (\(a,_) -> a == x) l

reversePairFilterVar :: Char -> [(Char, Int)] -> [(Char, Int)] -- Receives a list of tuples (var,Exp) and a char and returns a list containing only the tuples that dont have the char as their var
reversePairFilterVar x l = if (length f == 0) then [] else f
  where f = filter (\(a,_) -> a /= x) l

unzipNormVars :: [(Char,Int)] -> String -- Receives a list of tuples (var,exp) and returns a string containing all the vars
unzipNormVars [] = []
unzipNormVars ((a,b):ls) = [a] ++  unzipNormVars ls

unzipNormExp :: [(Char,Int)] -> [Int] -- Receives a list of tuples (var,exp) and returns a list of all the exponents
unzipNormExp [] = []
unzipNormExp ((a,b):ls) = [b] ++  unzipNormExp ls

mulPolStep :: Mon -> Poly -> Poly -- Intermediate funciton of multiplication, multiplies one monomial with a polynomial (to be called for every monomial of polynomial 1 of multiplication to be multiplied with polynomial 2)
mulPolStep _ [] = []
mulPolStep m (l:ls) = [(mulMon m l)] ++ mulPolStep m ls

mulPol :: Poly -> Poly -> Poly -- Multiplies two polynomails by going through the first one recursively and calling the function that multiplies each monomial with the other polynomial,ex : (1 + 2) * (3 + 4) = (1*3) + (1*4) + (2*3) + (2*4)
mulPol [] _ = []
mulPol (p1:p1s) p2 = (mulPolStep p1 p2) ++ mulPol p1s p2

uiMulPol :: Poly -> Poly -> IO () -- Prints the resulting polynomial after both were multiplied
uiMulPol p1 p2 = if (length mP == 0) then (printPolIO "0") else printPolIO (if (getNum (head (mP)) > 0) then (getStrPol (mP)) else "-" ++ getStrPol (mP))
  where mP = normPol (mulPol p1 p2)

testMulPol :: Poly -> Poly -> String -- To be used in the tests section to be put inside a `putStr` and show the result of the test to the user
testMulPol p1 p2 = if (length mP == 0) then ("0") else (if (getNum (head (mP)) > 0) then (getStrPol (mP)) else "-" ++ getStrPol (mP))
  where mP = normPol (mulPol p1 p2)




--Derivation Section -Task d)

deriveMon :: Char -> Mon -> Mon -- Derives one Monomial with respect to a certain variable
deriveMon d (a,b,c) = 
  let varIndex = getVarIndex d (a,b,c)
      exp = getExp d (a,b,c)
      delVar = deleteVar d b

  in (if (existsVarMon d (a,b,c)) then (if (exp - 1 > 0) then (a * (exp),b, removeOneFromExp (varIndex) (zip c [0..])) else (if (delVar == "") then (a*exp,"~",[0]) else (a * (exp),delVar, deleteExp (varIndex) (zip c [0..])))) else (0,"~",[0]))  

existsVarMon :: Char -> Mon -> Bool --Checks to see if a given variable exists in a monomial, in derivation this is very useful since, ex: d/dx (7y) = 0 since the x is not present
existsVarMon c (_,s,_) = if (length (filter (\x -> x == c) s) > 0) then True else False

deleteVar :: Char -> String -> String -- Deletes a given variable from a string, useful when derivating since some variables may disappear
deleteVar c [] = []
deleteVar c (s:sx) = if (c == s) then (deleteVar c sx) else [s] ++ deleteVar c sx

deleteExp :: Int -> [(Int,Int)] -> [Int] --Deletes a exponent from a string given its index, useful when deriving and the variable is deleted 
deleteExp _ [] = []
deleteExp i ((a,b):ls) = if (i == b) then (deleteExp i ls) else [a] ++ deleteExp i ls  

removeOneFromExp :: Int -> [(Int,Int)] -> [Int] --this list is after a zip function and the first int is the index, ex: [(a,0),(b,1),(c,2)] where a.b.c, etc are the exponents, the function goes recursively through the list of (exponents,indexes) and substracts 1 from an exponent given an index (needed when deriving)
removeOneFromExp _ [] = []
removeOneFromExp i ((a,b):ls) = if (i == b) then ([a-1] ++ removeOneFromExp i ls) else [a] ++ removeOneFromExp i ls  

derivePoly :: Char -> Poly -> Poly -- Goes recursively through the polynomial and derives each member (monomial)
derivePoly _ [] = []
derivePoly c (p:ps) = [deriveMon c p] ++ derivePoly c ps

derivePolyNormalize :: Char -> Poly -> Poly --Normalizes the polynomial before and after deriving to make sure everything is in the correct format and cases like 4x + 7 = 4 wont show the 7 since it disappears
derivePolyNormalize c p = normPol (derivePoly c (normPol p))


uiDerivePol :: Char -> Poly -> IO () -- Prints the Polynomial after being derived and normalized, also given the string output format that we have chosen, we need to confirm whether the first monomial is positive or negative otherwise it will not have the "-" in the string
uiDerivePol c p = if (length dP == 0) then (printPolIO "0") else printPolIO (if (getNum (head (dP)) > 0) then getStrPol (dP) else "-" ++ getStrPol (dP))
  where dP = derivePolyNormalize c p

testDerivePol :: Char -> Poly -> String -- To be used in the tests section to be put inside a `putStr` and show the result of the test to the user
testDerivePol c p = if (length dP == 0) then ("0") else (if (getNum (head (dP)) > 0) then getStrPol (dP) else "-" ++ getStrPol (dP))
  where dP = derivePolyNormalize c p  


--Parsing String Input into Our Format Section

removeSpaces:: String -> String --Removes all spaces from a string
removeSpaces "" = ""
removeSpaces (' ':sx) = removeSpaces sx
removeSpaces (s:sx) = [s] ++ removeSpaces sx

getStringMember :: String -> String --Gets the first member from a string, ex f "-2x+6" = "-2x"
getStringMember "" = ""
getStringMember [a] = [a]
getStringMember (s:sx) = if (s == '+') then "" else (if ((head sx) == '-') then [s] else [s] ++ getStringMember sx)

removeFoundMember :: String-> String -> String --Takes away from a string the first n characters from a string where n is the length of the input, this is an auxiliary function used when parsing the string so to work with in the mother function we need to take away a "+" sign that may occur at the beggining
removeFoundMember found s = if (length x == 0) then [] else (if (head x == '+') then reverse (take (length x-1) (reverse x)) else x)
  where x = reverse (take (length s - length found) (reverse s))

strSplit :: String -> [String] --Goes recursively and appends to the final list the members found in the string by getting the first member and calling itself with the rest of the string, note that given the other functions, the "-" sign will be kept so that later on while parsing we know whether the number is positive or negative
strSplit [] = []
strSplit s = [fstMember] ++ strSplit (removeFoundMember (fstMember) s)
  where fstMember = getStringMember s

getNumFromString :: String -> String -- Receives a string and returns only the number (and possibly the '-' sign), ex: f "-22xy^3z^2" = "-22"
getNumFromString "" = ""
getNumFromString (s:sx) = if ((ord s > 47 && ord s < 58) || s == '-') then [s] ++ getNumFromString sx else ""

getVarFromString :: String -> String --Receives a string and returns only the variables
getVarFromString "" = ""
getVarFromString (s:sx) = if (ord s >= 97 && ord s <= 122) then ([s] ++ getVarFromString sx) else "" ++ getVarFromString sx

getExpFromString :: String -> [Int] --Receives a string and each time it sees a '^' sign, it uses the getNumFromString function to get the exponent corresponding to that variable and appends it to the list
getExpFromString "" = []
getExpFromString [a] = if (ord a >= 97 && ord a <= 122) then [1] else []
getExpFromString (s:sx) = if (ord s >= 97 && ord s <= 122 && (head sx) /= '^') then [1] ++ getExpFromString sx else (if (s == '^') then [read (getNumFromString sx)::Int] ++ getExpFromString sx else getExpFromString sx)


createMon :: String -> Mon --Receives a string and uses auxiliary functions to convert it to a monomial
createMon s = 
  let var = getVarFromString s
      num = if (length numString >0) then (if (numString == "-") then -1 else (read (getNumFromString s)::Int)) else 1
      numString = getNumFromString s
  in if (var=="") then (num,"~",[0]) else (num,getVarFromString s,getExpFromString s)      

createPol :: [String]-> Poly --Receives a list of strings and recursively creates monomials and appends them to generate a polynomial
createPol [] = []
createPol (l:ls) = [createMon l] ++ createPol ls

polyParse :: String -> Poly --Receives a string and converts it into a list of strings containing each member of the future polynomial and then converts that list into a polynomial
polyParse s = createPol (strSplit (removeSpaces s))


-- User Interface Section


programUI :: IO ()
programUI = do
  putStr "\n\nPolynomial Operations Calculator \n\n\n What would you like to do? \n\n 1. Normalize a polynomial \n 2. Sum two polynomials \n 3. Multiply two polynomials \n 4. Derive a polynomial\n\n"
  option <- getLine
  if (option == (show 1)) then
    do
    putStr "\nPlease write the polynomial you would like to normalize in the form 'ax^2 + bx + c'\n\n"
    poly <- getLine
    uiNormPol (polyParse poly)
    putStr "\nWould you like to go back?\n[y/n]\n"
    
    back <- getLine
    if (back == "y") then
      programUI
    else
      putStr "\nThank you for using this program!\n"  
  else 
    if (option == (show 2)) then 
      do
      putStr "\nPlease write the two polynomials you would like to sum in the form 'ax^2 + bx + c'\n\n"
      poly1 <- getLine
      poly2 <- getLine
      uiSumPol (polyParse poly1) (polyParse poly2)
      putStr "\nWould you like to go back?\n[y/n]\n"
      
      back <- getLine
      if (back == "y") then
        programUI
      else
        putStr "\nThank you for using this program!\n"
    else 
      if (option == (show 3)) then
        do
        putStr "\nPlease write the two polynomials you would like to multiply in the form 'ax^2 + bx + c'\n\n"
        poly1 <- getLine
        poly2 <- getLine
        uiMulPol (polyParse poly1) (polyParse poly2)
        putStr "\nWould you like to go back?\n[y/n]\n"
        
        back <- getLine
        if (back == "y") then
          programUI
        else
          putStr "\nThank you for using this program!\n"
      else    
        if (option == (show 4)) then
          do
          putStr "\nPlease write the polynomials you would like to derive in the form 'ax^2 + bx + c' and the variable with respect to which you want to derivate\n\n"
          poly1 <- getLine
          var <- getLine
          uiDerivePol (head var) (polyParse poly1)
          putStr "\nWould you like to go back?\n[y/n]\n"
          
          back <- getLine
          if (back == "y") then
            programUI
          else
            putStr "\nThank you for using this program!\n"
        else     
          putStr "\nThank you for using this program!\n"   




-- QuickCheck seciton
