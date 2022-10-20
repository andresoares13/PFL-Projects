import Project1 (programUI,testNormPol,testSumPol,testDerivePol,testMulPol,polyParse)


tests :: IO ()
tests = do
    putStr "\nWelcome to the testing interface!\nWhat would you like to do?\n\n1. Open the program's user interface (to test with your own polynomials)\n\n2. Run the tests (note that the expected values are hardcoded in the code and were calculated by WolframAlpha)\n\n" 
    option <- getLine
    if (option == (show 1)) then
        programUI
    else
        if (option == (show 2)) then
            do 
              putStr ("\n/// Testing Normalization ///\n\n" ++ "1. Polynomial = 0x^2 + 2y + 5z + y + 7y^2\n\n" ++"Result:   " ++ (testNormPol (polyParse "0x^2 + 2y + 5z + y + 7y^2")) ++ "\n\nExpected:   7y^2 + 3y + 5z\n\n" ++ "2. Polynomial = 8x^2 + 9y - 4z^3 + 7yx - 7yx + 2\n\nResult:   " ++ (testNormPol (polyParse "8x^2 +  9y  -4z^3 +7yx-7yx+2")) ++ "\n\nExpected:   -4z^3 + 8x^2 + 9y + 2\n\n")
              putStr ("\n/// Testing Sum ///\n\n" ++ "1. Polynomials -> (3x^2 + 6x + 4y^2 + 5x)  +  (7x - 6y^2 + 4x)\n\n" ++ "Result:   " ++ testSumPol (polyParse "3x^2+6x+4y^2+5x") (polyParse "7x-6y^2+4x") ++ "\n\nExpected:   3x^2 - 2y^2 + 22x\n\n")
              putStr ("2. Polynomials -> (3x^2 + 6x + 4y^2 + 5x)  +  (7x - 6y^2 + 4xy^2)\n\n" ++ "Result:   " ++ testSumPol (polyParse "3x^2 + 6x + 4y^2 +5x") (polyParse "7x -6y^2 +4xy^2") ++ "\n\nExpected:   3x^2 + 4xy^2 - 2y^2 + 18x\n\n")
              putStr ("3. Polynomials -> (7x - 6y^2 + 4x + 5)  +  (y)\n\n" ++ "Result:   " ++ testSumPol (polyParse "7x -6y^2 +4x+5") (polyParse "y") ++ "\n\nExpected:   -6y^2 + 11x + y + 5\n\n")
              putStr ("4. Polynomials -> (10x^2 - 10y^2 - x - 10x^2 + 5)  +  (5z^6 + 10y^2 - 19 + 4x^19 - 7x^4)\n\n" ++ "Result:   " ++ testSumPol (polyParse "10x^2 - 10y^2 - x - 10x^2 + 5") (polyParse "5z^6 + 10y^2 - 19 + 4x^19 - 7x^4") ++ "\n\nExpected:   4x^19 + 5z^6 - 7w^4 - x - 14\n\n")
              putStr ("\n/// Testing Multiplication ///\n\n" ++ "1. Polynomials -> (2x^2 + 3y)  *  (3y^3 + 4zx)\n\n" ++ "Result:   " ++ testMulPol (polyParse "2x^2 + 3y") (polyParse "3y^3 + 4zx") ++ "\n\nExpected:   9y^4 + 6x^2y^3 + 8x^3z + 12xyz\n\n")
              putStr ("\n/// Testing Derivation ///\n\n" ++ "1. Polynomial -> (7x - 6y^2 + 4x + 5)  deriving in regard to 'x'\n\n" ++ "Result:   " ++ (testDerivePol 'x' (polyParse "7x - 6y^2 + 4x + 5")) ++ "\n\nExpected:   11\n\n")
              putStr ("2. Polynomial -> (2x^2y^3 + 4x^2 + 8y - 9zx)  deriving in regard to 'x'\n\n" ++ "Result:   " ++ (testDerivePol 'x' (polyParse "2x^2y^3 + 4x^2 + 8y-9zx")) ++ "\n\nExpected:   4xy^3 + 8x - 9z\n\n")
              putStr ("3. Polynomial -> (3x + 4y + 2xy + 1)  deriving in regard to 'x'\n\n" ++ "Result:   " ++ (testDerivePol 'x' (polyParse "3x + 4y + 2xy + 1")) ++ "\n\nExpected:   2y + 3\n\n")
              putStr ("4. Polynomial -> (3x + 4y + 2xy + 1)  deriving in regard to 'y'\n\n" ++ "Result:   " ++ (testDerivePol 'y' (polyParse "3x + 4y + 2xy + 1")) ++ "\n\nExpected:   2x + 4\n\n")
              putStr ("\n/// Testing Combination of Operations ///\n\n" ++ "1. Polynomials -> (2xyz)  +  ((2x^2 + 3y)  *  (3y^3 + 4zx))\n\n" ++ "Result:   " ++ (testSumPol (polyParse (testMulPol (polyParse "2x^2 + 3y") (polyParse "3y^3 + 4zx"))) (polyParse "2xyz")) ++ "\n\nExpected:   9y^4 + 6x^2y^3 + 8x^3z + 14xyz\n\n")
              putStr ("2. Polynomials -> (80y^2x + 42) + (((norm 8x^2 + 9y - 4z^3 + 7yx - 7yx + 0) deriving in regard to 'x') * (17yx + y^2))\n\n" ++ "Result:   " ++ (testSumPol (polyParse (testMulPol (polyParse (testDerivePol 'x' (polyParse (testNormPol (polyParse "8x^2 +  9y  -   4z^3 +7yx-7yx+0"))))) (polyParse "17yx+   5y^2"))) (polyParse "80y^2x + 42")) ++ "\n\nExpected:   272x^2y + 160xy^2 + 42\n\n")
              putStr ("\nEnd of testing\nPlease scroll to the top to see all the tests\n\n")
        else
            putStr "\nThank you for using this program!\n"


          

            

--Tests Section -- test list in old format

--test of norm: 
              --uiNormPol[(0,"x",[2]),(2,"y",[1]),(5,"z",[1]),(1,"y",[1]),(7,"y",[2])
              --uiNormPol (polyParse "8x^2 +  9y  -4z^3 +7yx-7yx+2")
-- test of sum: 
              --uiSumPol [(3,"x",[2]),(6,"x",[1]),(4,"y",[2]),(5,"x",[1])] [(7,"x",[1]),((-6),"y",[2]),(4,"x",[1])]
              --uiSumPol [(3,"x",[2]),(6,"x",[1]),(4,"y",[2]),(5,"x",[1])] [(7,"x",[1]),((-6),"y",[2]),(4,"xy",[1,2])]
              --uiSumPol [(7,"x",[1]),((-6),"y",[2]),(4,"x",[1]),(5,"~",[0])] [(1,"y",[1])]
              --uiSumPol [(10,"x",[2]),((-10),"y",[2]),((-1),"x",[1]),((-10),"x",[2]),(5,"~",[0])] [(5,"z",[6]),(10,"y",[2]),((-19),"~",[0]),(4,"x",[19]),((-7),"w",[4])]
              --uiSumPol (mulPol [(2,"x",[2]),(3,"y",[1])] [(3,"y",[3]),(4,"zx",[1,1])]) [(2,"xzy",[1,1,1])
-- Test of mul:
              --uiMulPol [(2,"x",[2]),(3,"y",[1])] [(3,"y",[3]),(4,"zx",[1,1])]

-- test of derivation:
              --uiDerivePol 'x' [(7,"x",[1]),((-6),"y",[2]),(4,"x",[1]),(5,"~",[0])] 
              --uiDerivePol 'x' [(2,"xy",[2,3]),(4,"x",[2]),(8,"y",[1]),((-9),"zx",[1,1])]

-- test of everything together (even parsing):
              --uiSumPol (mulPol (derivePoly 'x' (normPol (polyParse "8x^2 +  9y  -   4z^3 +7yx-7yx+0"))) (polyParse "17yx+   5y^2")) (polyParse "80y^2x + 42")              

