--Version of GHCi used -> 9.0.1
--Version of cabal used -> 3.4.0.0

import Numeric.Natural
import Test.QuickCheck
import GHC.Arr (done)
import Test.QuickCheck.Property (Prop)
import Test.QuickCheck (Arbitrary)

-------------------------------------------------------------------
--Official factorial function
fac :: Natural -> Natural 
fac n = product[1..n]

-------------------------------------------------------------------

--fac1 : Another Junior Haskell programmer factorial function
fac1 :: Natural -> Natural 
fac1 0 = 1
fac1 n = n * fac (n-1)  

--On line 19 we defined that the factorial of 0 is 1, this holds by the matematical definition.
--This code line is the stop statment of the recursion.
--On line 20 we define that the factorial of a number n is that number times its predecessor
--This hols because we're working over the Natural set, so we can now what is the
--predecesor of a determinate number. 

-------------------------------------------------------------------

--fac2 : "Points free" Haskell programmer factorial function
fac2 :: Natural -> Natural 
fac2 = foldr (*) 1 . enumFromTo 1

--On line 32 the function enumFromTo creates a list starting on 1 and going  
--The function foldr takes each element of a list starting from the last one and then aplies the operation
--that the argument indicates. 

-------------------------------------------------------------------

--fac3 : Iterative Haskell programmer factorial function
fac3 :: Natural -> Natural
fac3 n = result (for init next done)
        where init = (0,1)
              next   (i,m) = (i+1, m * (i+1))
              done   (i,_) = i==n
              result (_,m) = m

for i n d = until d n i

-------------------------------------------------------------------

--fac4 : Memoizing Haskell programmer
fac4 :: Natural ->Natural  
fac4 = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))

-------------------------------------------------------------------

--Acumulating Haskell programmer factorial function
fac5 :: Natural->Natural 
facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)
fac5 = facAcc 1

-------------------------------------------------------------------

--testing 
instance Arbitrary Natural where
      arbitrary = arbitrarySizedNatural 
      shrink = shrinkIntegral

prop_fac :: (Natural -> Natural) -> Natural -> Bool  
prop_fac list n = list n == fac n

f :: [Natural -> Natural] -> Natural -> Bool 
f [] k = True 
f (x:xs) k = prop_fac x k && f xs k

main :: IO ()
main = quickCheck $ f[fac1,fac2,fac3,fac4,fac5]