--
-- CS 430 Spring 2019 P3 (Haskell 1)
--
-- Name: James Schader
-- 20220225
--

module P3 where

-- A list of all factors of n.
-- taken from StackOverflow, notgiorgi
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]
-- COMPLETE

-- True iff n is prime.
isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1,n]
-- COMPLETE

-- A list of all prime factors of n.
primeFactors :: Integral a => a -> [a]
primeFactors n = filter (\x -> isPrime x) (factors n)
-- COMPLETE

-- A list of primes up to n.
primesUpTo :: Integral a => a -> [a]
primesUpTo n
  | n == 1 = []
  | isPrime n = [n] ++ primesUpTo (n-1)
  | otherwise = primesUpTo (n-1)
-- COMPLETE

-- True iff n is a perfect number.
-- A number n is perfect if the sum of its factors is 2*n.
isPerfect :: Integral a => a -> Bool
isPerfect n = 2*n == sum (factors n)
-- COMPLETE

-- A list of all perfect numbers up to n.
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = filter isPerfect [1..n]
-- COMPLETE

-- The next prime greater than n.
nextPrime :: Integral a => a -> a
nextPrime n
  | isPrime (n+1) = n+1
  | otherwise = nextPrime (n+1)
-- COMPLETE

-- A list of the first n primes.
generatePrimes :: Integral a => a -> [a]
generatePrimes n
  | n > 1 = generatePrimes (n-1) : nextPrime (last (generatePrimes (n-1)))
  | n ==1 = 2
  | n < 1 = []
-- NOT DONE
