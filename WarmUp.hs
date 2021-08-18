--Version of GHCi used -> 9.0.1
--Version of cabal used -> 3.4.0.0

import Numeric.Natural
import Test.QuickCheck
import GHC.Arr (done)
import Test.QuickCheck.Property (Prop)
import Test.QuickCheck (Arbitrary)

--------------------------------------------------------------------------------------------------------------------------------------
--Official factorial function
fac :: Natural -> Natural 
fac n = product[1..n]

--------------------------------------------------------------------------------------------------------------------------------------

--fac1 : Another Junior Haskell programmer factorial function
fac1 :: Natural -> Natural 
fac1 0 = 1
fac1 n = n * fac (n-1)  

-- |The function is defined from Naturals to Naturals

--On line 19 we defined that the factorial of 0 is 1, this holds by the matematical definition.
--This code line is the stop statment of the recursion.
--On line 20 we define that the factorial of a number n is that number times its predecessor
--This hols because we're working over the Natural set, so we can now what is the
--predecesor of a determinate number. 

--------------------------------------------------------------------------------------------------------------------------------------

--fac2 : "Points free" Haskell programmer factorial function
fac2 :: Natural -> Natural 
fac2 = foldr (*) 1 . enumFromTo 1

-- |The fuction is defined from Naturals to Naturals

--On line 32 the function enumFromTo creates a list starting on 1 and ending on the number n that the
--funciton will use to calculate the factorial 
--The function foldr takes each element of the list that enum return starting from the last one and 
--then aplies the operation that the argument indicates. 

--------------------------------------------------------------------------------------------------------------------------------------

--fac3 : Iterative Haskell programmer factorial function
fac3 :: Natural -> Natural
fac3 n = result (for init next done)
        where init = (0,1)
              next   (i,m) = (i+1, m * (i+1))
              done   (i,_) = i==n
              result (_,m) = m

for i n d = until d n i

-- |The function is defined from Natural to Natural
--On this function we define an inatial value, this value is the starting position for the factorial.
--Then we use next to do the factorial, mltipling the initial value times that value plus 1. Done is 
--the indicator that decides when the function stops.

--------------------------------------------------------------------------------------------------------------------------------------

--fac4 : Memoizing Haskell programmer
fac4 :: Natural ->Natural  
fac4 = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))

-- |The function is definede from Naturals to Naturals

--This function recives a number n, then if n = 0, the function will return 1. Otherise it will return the 
--multiplication of the number n times n-1 until n = 0.

--------------------------------------------------------------------------------------------------------------------------------------

--Acumulating Haskell programmer factorial function
fac5 :: Natural->Natural 
facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)
fac5 = facAcc 1

-- |The function is defined from Naturals to Naturals

--This function have an accumulator, when the number n that the funcition recives take the value of 0
--the function return the value of the accumulator at that moment. That value is changed when the value 
--of n is decreased by 1.

--------------------------------------------------------------------------------------------------------------------------------------

--Testing

instance Arbitrary Natural where
      arbitrary = arbitrarySizedNatural 
      shrink = shrinkIntegral

prop_fac :: (Natural -> Natural) -> Natural -> Bool  
prop_fac list n = list n == fac n

-- |This function is definede from Naturals to Naturals and it restur us a Boolean value
--this function recives two arguments a list and a number, then it compares the function
--that we use as parameter and the official factorial function using the other parameter

f :: [Natural -> Natural] -> Natural -> Bool 
f [] k = True 
f (x:xs) k = prop_fac x k && f xs k

-- |This funcrion is defined from a list of Naturals to Naturals, to Naturals and returnus a Boolean vauel
--on thsi function we define a list of our factorial functions and recursively use the prop_fac function
--to prove they're correcto.

main :: IO ()
main = quickCheck $ f[fac1,fac2,fac3,fac4,fac5]

--Testing was done with QuickCheck
--------------------------------------------------------------------------------------------------------------------------------------