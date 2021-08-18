-- Representation of the factorial function  with different methods using functional programming.

-- Maria Alejandra Vélez Clavijo y Alejandra Palacio Jaramillo.

--[1] Fritz Ruehr. The Evolution of a Haskell Programmer, n.d. http://www.willamette.edu/~fruehr/haskell/evolution.html

-- Windows 10.
-- Tested with GHC 9.0.1 and QuickCheck 2.14.2

module WarmUp where 

import Test.QuickCheck
    ( Arbitrary(arbitrary)
    , classify
    , collect
    , elements
    , Property
    , quickCheck
    )
import Numeric.Natural ( Natural )
----------------------------------------------------------------------------------------------------------------------
-- Implementations of the factorial function.

-- [1] The Evolution of a Haskell Programmer: Freshman Haskell programmer.
fac1 :: Natural -> Natural 
fac1 n = if n == 0 
           then 1
           else n * fac1 (n-1)

-- [1] The Evolution of a Haskell Programmer: Junior Haskell programmer.
fac2 :: Natural -> Natural 
fac2  0    =  1
fac2 (n+1) = (n+1) * fac2 n
 
-- [1] The Evolution of a Haskell Programmer: Another junior Haskell programmer.
fac3 :: Natural -> Natural 
fac3 0 = 1
fac3 n = n * fac3 (n-1)

-- [1] The Evolution of a Haskell Programmer: Senior Haskell programmer.
fac4 :: Natural -> Natural
fac4 n = foldr (*) 1 [1..n]

-- [1] The Evolution of a Haskell Programmer: Sophomore Haskell programmer, at MIT.
fac5 :: Natural -> Natural
fac5 = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))

-- Our official implementation of the factorial function.
fac :: Natural -> Natural 
fac n = product [1..n]

-- Properties

prop1 :: Natural -> Bool 
prop1 n = fac1 n == fac n

prop2 :: Natural -> Bool 
prop2 n = fac2 n == fac n

prop3 :: Natural -> Bool 
prop3 n = fac3 n == fac n

prop4 :: Natural -> Bool 
prop4 n = fac4 n == fac n

prop5 :: Natural -> Bool 
prop5 n = fac5 n == fac n

prop_fac :: [a] -> Natural -> Bool 
prop_fac [] = False
prop_fac (x:xs) = fac n == x n == prop_fac(xs)