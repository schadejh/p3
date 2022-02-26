--
-- CS 430 Spring 2019 P3 (Haskell 1)
--
-- Name: James Schader
-- 20220225
--

module P3 where

-- A list of all factors of n.
factors :: Integral a => a -> [a]
factors n
  | n < 1 = []
  | n ==1 = [1]
  | otherwise = [-1] -- NOT DONE

-- True iff n is prime.
isPrime :: Integral a => a -> Bool
isPrime n
  | n < 2 = False
  | otherwise = True -- NOT DONE

-- A list of all prime factors of n.
primeFactors :: Integral a => a -> [a]
primeFactors n = (\n -> isPrime n) factors n
-- COMPLETE

-- A list of primes up to n.
primesUpTo :: Integral a => a -> [a]
primesUpTo n = []
-- loop 0..n
--   if i isPrime, append to list
-- end loop
-- return list

-- True iff n is a perfect number.
-- A number n is perfect if the sum of its factors is 2*n.
isPerfect :: Integral a => a -> Bool
isPerfect n = 2*n == sum factors n
-- COMPLETE

-- A list of all perfect numbers up to n.
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = filter (\x -> isPerfect x) [0..n]
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
  | n < 1 = []
  | n ==1 = [2]
-- NOT DONE
