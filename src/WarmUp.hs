-- Representation of the factorial function  with different methods using functional programming.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

--[1] Fritz Ruehr. The Evolution of a Haskell Programmer, n.d. http://www.willamette.edu/~fruehr/haskell/evolution.html

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2

module WarmUp where 

import Test.QuickCheck
import Numeric.Natural ( Natural )
------------------------------------------------------------------------------------------------------------------------
-- Implementations of the factorial function.

-- | This factorial function is defined from Naturals to Naturals. First, the function evaluate the input number. If this
-- number is equals to zero, then the factorial function result is 1. If the number is different to zero, the factorial 
-- function result will be the number multiplied with the factorial of the previous number. In conclusion, the factorial 
-- of the number is going to be calcuated recursively.

-- [1] The Evolution of a Haskell Programmer: Freshman Haskell programmer.
fac1 :: Natural -> Natural 
fac1 n = if n == 0 
           then 1
           else n * fac1 (n-1)

-- | This factorial function is defined from Naturals to Naturals. The foldr method takes the second argument and the last
-- item of the list (the list given by the enumFromTo method -a list from 1 to n-) and applies the function (*). Then it  
-- takes the penultimate item from the end and the result, and successively until the factorial is obtained. 

-- [1] The Evolution of a Haskell Programmer: "Points-free" Haskell programmer.
fac2 :: Natural -> Natural 
fac2 = foldr (*) 1 . enumFromTo 1
 
-- | This factorial function is defined from Naturals to Naturals. First, this factorial function defined the basis case
-- (when the number is zero, the result is one) and then, if the number is different to zero, the function calculated the
-- factorial multiplying the number with the factorial of the previous number until getting the basis case again. 
-- The final result is obtained recursively.

-- [1] The Evolution of a Haskell Programmer: Another junior Haskell programmer.
fac3 :: Natural -> Natural 
fac3 0 = 1
fac3 n = n * fac3 (n-1)

-- | This factorial function is defined from Naturals to Naturals. The foldr method takes the second argument and the last
-- item of the list (the list from 1 to n) and applies the function (*). Then it takes 
-- the penultimate item from the end and the result, and successively until the factorial is obtained. 

-- [1] The Evolution of a Haskell Programmer: Senior Haskell programmer.
fac4 :: Natural -> Natural
fac4 n = foldr (*) 1 [1..n]

-- | This factorial function is defined from Naturals to Naturals. The function evaluate the input number and if this
-- number is equals to zero, then the factorial function  is 1. In another case, the factorial 
-- function result will be the number multiplied with the factorial of the previous number. In conclusion, the factorial 
-- of the number is going to be calcuated recursively and is similar to fac1.

-- [1] The Evolution of a Haskell Programmer: Sophomore Haskell programmer, at MIT.
fac5 :: Natural -> Natural
fac5 = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))

-- Our official course implementation of the factorial function.
fac :: Natural -> Natural 
fac n = product [1..n]

------------------------------------------------------------------------------------------------------------------------
-- | Property: Our prop_fact is defined from a function list, that is from Naturals to Naturals, to Naturals to Boolean. 
-- First, the function evaluates the basis case: when the list is empty, the result will be true. Then, is defined the 
-- general case: the function takes the head of the list and compares the result of that function with the factorial obtained 
-- with the official course implementation. At the same time, the function calculate and compare recursively with the tail 
-- of the list the factorial like in the previous step until the basis case. If the result is true, the prop_fact works well
-- and the test for the others factorial functions is correct.

prop_fact :: [Natural -> Natural]-> Natural -> Bool 
prop_fact [] n = True 
prop_fact (x:xs) n = (fac n == x n ) == prop_fact xs n

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

main :: IO ()
main = quickCheck $ prop_fact[fac1,fac3,fac4,fac5]

