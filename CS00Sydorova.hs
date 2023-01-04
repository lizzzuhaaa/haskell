{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module CS00Sydorova where

-- Task 1 -----------------------------------------
multy35 :: Int 
multy35 = sum (division 3)
        where   division n = if n == 1000 then []
                else if ((n `mod` 3) == 0) || ((n `mod` 5) == 0) then [ n ] ++ division(n+1)
                else division(n+1)

-- Task 2 -----------------------------------------
fibonacciEv :: Int
fibonacciEv = sum [a | a <- fiblist [0,1], (a `mod` 2) == 0 ]
         where  fiblist :: [Int] -> [Int]
                fiblist n = if n!!(length n - 1) >= 400000 then n
                else  fiblist (n ++ [fib (length n)])
                         where  fib :: Int -> Int
                                fib k = if k == 0 then 0 else if k == 1 then 1 else fib (k-1) + fib (k-2)

-- Task 3 -----------------------------------------
largestPrimeFactor :: Int
largestPrimeFactor = largestPrFact (600851475143)(2)
                where   largestPrFact :: Int -> Int -> Int
                        largestPrFact n k = if k==n then k
                        else if ((n `mod` k) == 0) then largestPrFact (n `div` k)(2)
                        else largestPrFact (n)(k+1)

-- Task 4 -----------------------------------------
largestpalindrome :: Int
largestpalindrome = palindrome (999)(999)
                where   palindrome :: Int -> Int -> Int
                        palindrome k n = if n < 900 then palindrome(k-1)(999)
                        else if tolist (show(k*n)) == (reverse (tolist (show(k*n)))) then k*n
                        else palindrome(k)(n-1)
                                        where   tolist:: String -> [Char]
                                                tolist a = if null a then []
                                                else [head a] ++ tolist (tail a)

-- Task 5 -----------------------------------------
listDiv :: [Int]
listDiv = [1..20]

smallestMultiple :: Int
smallestMultiple = findNum (2520) (listDiv)
                where   findNum :: Int -> [Int] -> Int 
                        findNum n l = if null l then n
                        else findNum((n*(head l)) `div` (gcd(n)(head l)))(tail l)
                                
-- Task 6 -----------------------------------------        
listH :: [Int]
listH = [1..100]

sumSqDiff :: Int
sumSqDiff = sum(listH) * sum(listH) - sum (sqlist)
        where   sqlist :: [Int]
                sqlist = [n| a <-listH, n<- [a*a]]

-- Task 7 -----------------------------------------
prime :: Int
prime = (listPrime [2..]) !! (10000)
                where   listPrime :: [Int] -> [Int]
                        listPrime xs = [head xs] ++ listPrime [x | x<-(tail xs), (x `mod` (head xs)) > 0] 

--Task 8 -----------------------------------------
largestprod :: Integer
largestprod = maximum (prod (listN (num)) )
                where   prod :: [Integer] -> [Integer]
                        prod xs = if length(take 13 xs) < 13 then []
                        else [product (take 13 xs)] ++ prod (tail xs)

listN :: Integer -> [Integer]
listN n = if n == 0 then [] else listN(n `div` 10) ++ [n `mod` 10]

num :: Integer
num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450 

--Task 9 -----------------------------------------
specialPythagoreanTriplet :: Integer
specialPythagoreanTriplet = product pyth
                                where   pyth :: [Integer]
                                        pyth = head [[a, b, c] | k <- [2..(1000-2)], m <- [1..(1000-3)],
                                                                let a = k^2 - m^2 , let b = 2*k*m, let c =  k^2 - m^2,
                                                                a + b + c == 1000 ]

--Task 10 -----------------------------------------
sumPrimes :: Integer
sumPrimes =  sum (listPrime [2..])
                where   listPrime :: [Integer] -> [Integer]
                        listPrime xs = [head xs] ++ listPrime [x | x<-(tail xs), (x `mod` (head xs)) > 0, x < 200000] 