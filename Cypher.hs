import Data.Char

-- a to z
dict = ['a'..'z']
-- FREQUENCY TABLE OF LETTERS IN ALPHABET
table :: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,
         0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
         6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

--Turns a letter in a corresponding number
let2nat:: Char -> Int
--Turns a number into a correspondeing letter
nat2let :: Int -> Char
--Shifts a letter over based on number put in
shift :: Int -> Char -> Char
--Uses Shift function to encode a given string
encode :: Int -> String -> String
--Decodes encoded strings
decode :: Int -> String -> String
--Returns the number of lower case aplhabetic chars in a string
lowers :: String -> Int
--Counts the number of times a char is within a string 
count :: Char -> String -> Int
-- Given two ints returns float percentage
percent :: Int -> Int -> Float
-- Given a string returns list of the frequency of letters in that string
freqs :: String -> [Float]
--Given a string rotate the letters x places, wrap at the end.
rotate :: Int ->[a] -> [a]
-- calculates the chi square statistic for a list of observed frequencies "os" with respect to a list of expected frequencies "es"
chisqr :: [Float] -> [Float] -> Float
-- returns the ﬁrst position (counting from zero) at which a value occurs in a list, assuming that it occurs at least once
position :: Float -> [Float] -> Int
--Find the minimum position in a list
minpos :: [Float] -> Int
-- Cracks ceaser cypher
crack :: String -> String

--LET2NAT
let2nat x = sum([1|x<-['a'..x]])-1;

--NAT2LET
nat2let y = helper y dict;

helper y x 
 | y/=0 = helper (y-1) (tail(x))
 | y==0 = head(x);

--SHIFT
shift x y 
 | (x>=(-25) && x<=25) = checkUpper x y
 | otherwise = y
 
checkUpper x y 
  |((ord y)<= 122 && (ord y) >= 97) = nat2let((x + let2nat(y)) `mod` 26) 
  | otherwise = y

--ENCODE
encode y [] = [];
encode y (x:xs) = shift y x : encode y xs;

--DECODE
decode y [] = [];
decode y (x:xs) = shift (0-y) x : decode y xs;

--LOWERS
lowers [] = 0;
lowers (x:xs) 
 |((ord x)<= 122 && (ord x) >= 97) = (1 + lowers (xs))
 | otherwise = lowers(xs)
 
-- COUNT
count y [] = 0
count y (x:xs)
 |(y==x) = 1+ count y (xs)
 | otherwise = count y (xs)

--PERCENT
percent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float


--FREQS
freqs x = [(percent (a)(lowers(x)))| a<-freqHelp(x)]

freqHelp x = [count a x| a<-['a'..'z']];

--ROTATE
rotate x y = (rotateHelp x y) ++ (take x y)

rotateHelp x [] = []
rotateHelp x (y:ys)
  | (x>0) = rotateHelp (x-1) ys
  | (x==0) = y:rotateHelp x ys
  
--CHISQR
chisqr x y = sum(chisqrHelp x y)

chisqrHelp [] [] = [];
chisqrHelp (x:xs) (y:ys) 
  |y/=0 = (((x-y)^2)/(y)): (chisqrHelp (xs) (ys))
  |y==0 = 0:(chisqrHelp (xs) (ys))
  
--Position
position x y = positionHelp x y 0;

positionHelp x [] z = (-1)

positionHelp x (y:ys) z
 |(x==y) = z
 |(x/=y) = positionHelp x ys (z+1)
 

--MINPOS
minpos y = position(minimum (y)) y 


--Crack
crack x = decode(minpos(crackHelp2(crackHelp1 x)table)) x

--frequency of each letter in each rotation
crackHelp1 y = [rotate x (freqs(y))| x<-[0..25], x<length(y)]

-- chisqr of each rotation frequency 
crackHelp2 x y =[chisqr a y| a<-x] 
