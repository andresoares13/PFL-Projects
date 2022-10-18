import Project1 (programUI,uiNormPol,uiSumPol,uiDerivePol,uiMulPol)
import Test.QuickCheck

tests :: IO ()
tests = do
    putStr "\nWelcome to the testing interface!\nWhat would you like to do?\n\n1. Open the program's user interface (to test with your own polynomials)\n\n2. Run the tests\n\n" 
    option <- getLine
    if (option == (show 1)) then
        programUI
    else
        if (option == (show 2)) then
            putStr "\nSoon\n"
        else
            putStr "\nThank you for using this program!\n"


          

            
                   
--Tests Section

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

