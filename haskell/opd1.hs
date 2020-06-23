module Opd1
where
import Data.Char
import Data.List

import System.IO  
import Control.Monad



faca::Int->Int
faca 0=1
faca x = x * faca (x-1)

facb::Int->Int  
facb x 
    | x == 0 = 1 
    | x == 1 = 1 
    | x == 2 = 2 
    | x == 3 = 6 
    | otherwise = x * facb (x-1) 

--Not a Number = NaN
nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c = [x1,x2]
        where
            d = b * b - 4 * a * c
            x1 = (-b + sqrt d) / (2 * a)
            x2 = ((- b) - sqrt d) / (2 * a)


nulpuntenb::Double->Double->Double->[Double]
nulpuntenb a b c 
    | delta < 0 = [] 
    | delta == 0 = [-b/2*a] 
    | delta > 0 = [-b/(2*a) + radix/(2*a), -b/(2*a) - radix/(2*a)]

    where
       delta = b*b-4*a*c
       radix = sqrt delta

multa::Int->[Int]->[Int]
multa _ [] = []
multa x (h:t) = (x*h):multa x t 

third::(a,b,c) ->c
third (_,_,c) = c

apply::(Int->Int->Int) -> Int -> [Int]->[Int]
apply f x [] = []
apply f x (head:tail) = (f x head) :apply f x tail

apply2::(Int->Int) -> [Int] -> [Int]
apply2 f [] = []
apply2 f (x:xs) = f x  : apply2 f xs

appl::(Int->Int) -> [Int]->[Int]
appl _ [] = []
appl f (x:xs) = (f x) :appl f xs

genapply::(a->b)->[a]->[b]
genapply _ [] = []
genapply f (x:xs) = (f x) :genapply f xs
-- map functie 


opd2c = [(x,y,z)|x<-[1..6], y<-[1..6],z<-[1..6], mod (x+y+z) 5 == 0]


--Dobbelstenen
opd2cc::Int
opd2cc = length opd2c

opd2d::Int -> Int
opd2d n = length[(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6], mod (x+y+z) n == 0]

--  opd2c = [(x,y,z)
--   | rest5 == 0 = x<-([1,2,3,4,6]),y<-([1..6]),z<-([1..6])]
--
--    where 
--       rest5 = mod (x+y+z) 5















-- OPDRACHTEN 2
-- 1a


euclid :: Integer -> Integer -> Integer
euclid 0 b = b
euclid a b = euclid (b `mod` a) a

-- 1b/gegeven; e en m gegeven, ( e*d=1 mod m), d in midden uitgespuugd
egcd :: Integer -> Integer -> (Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =
    let (g, s, t) = egcd (b `mod` a) a
         in (g, t - (b `div` a) * s, s)

-- Middelste result uit tuple
middle::(Integer,Integer,Integer)->Integer
middle (_,b,_) = b
    
-- 1b Versie die geen negtieven terug geeft
--ownegcd a b
--    |x <0 = 
--        let (g, s, t) = egcd (b `mod` a) a
--        in (g, t - (b `div` a) * s, s)
--    |otherwise = egcd a b
 --       where x = middle(egcd a b)

-- middle (_,b,_) = b 

-- 1b Versie die geen negtieven terug geeft
ownegcd::Integer -> Integer ->(Integer,Integer,Integer)
ownegcd a b
    |mid <0 = let (x, y, z) = oldEgcd
        in (x, y+b, z)              --b is modulo
        |otherwise = oldEgcd 
            where 
                oldEgcd = egcd a b
                mid = middle(oldEgcd)
        
mijnegcd e m
    |d<0=d+m
    | otherwise = d
    where d= middle $egcd e m


-- opdracht 2: sleutel generatie

-- check of een getal een priemgetal is
prime:: Integer -> Bool
prime n = (n>1) && all (\ x -> mod n x /= 0) [2..n-1]
 --1 is geen priem want heeft geen 2 verschillende getallen waar je het door kan delen
 -- Als bij overige getallen tussen 2 en n-1 de modulo niet 0 is, dan is het een priemgetal

showPrimes::[Integer]
showPrimes = [x
                |x<-[100..500], prime x]

  -- 2 priemgetallen kiezen
p = 157
q = 389
-- uit showPrimes
    
 --Modulus berekenen
m = p * q
 --is 61073
    
 --m' = Eulers totient functie : m0 = φ(m) = (p − 1) · (q − 1), m' = lengte van lijst getallen die  geen gemeenschappelijke factoren met modulus
m' = (p-1) * (q-1)
-- is 60528
    
--Geschikte e's teruggeven
--Vervolgens kiezen we een getal e dat relatief priem is met m'
--	Het getal e voldoet dus aan de volgende twee voorwaarden:
--		• e < m'
--		• ggd(e, m') = 1

showValidEs::[Integer]
showValidEs = [x
                   |x<-[1..m'], gcd x m' == 1]


e = 31903
-- uit bovenstaand gekozen
    
-- d (publieke sleutel) berekennen
d = middle(ownegcd e m')
--Equals 28159
    
    
--m is de modulus
--e is de private key
--d is de public key
    
--Encrypt
rsaEncrypt::(Integer,Integer)->Integer->Integer
rsaEncrypt (e, m) x = (x^e) `mod` m
    
--Decrypt
rsaDecrypt::(Integer,Integer)->Integer->Integer
rsaDecrypt (d, m) x = (x^d) `mod` m


---4: Versleutel en ontsleutel een letter m.b.v. de functies uit opdracht 3. Voor de conversie van een letter naar een ascii waarde en vice versa zijn twee handige
--functies beschikbaar:
-- • ord
-- • chr
--Oefen met beide functies 
        
--Char versleutelen
charEncrypt::Char->Integer
charEncrypt y = 
    let x = toInteger(ord y)
    in rsaEncrypt(e, m) x
        
--Char ontsleutelen
charDecrypt::Integer->Char
charDecrypt y = 
    let x = fromIntegral(rsaDecrypt (d, m) y)
    in chr x

---5:
    --Alice moet de public key gebruiken om te versleutelen, en de private key voor ontsleutelen (aan bob geven)
    -- eventueel dubbele versleuteling erna met bob's sleutels






first (a,_) = a
    
second (_,b) = b

telop:: Int-> Int -> Int
telop x y = x + y
telop2 x y = (+) x y 

neg::Int -> Int
neg x
    | x<0 = x
    | otherwise = -x

--restklasse2 :: Int->Int->Int
--restklasse2 = start modulus = [start,start+modulus..]
-- restklasse start modulus = [start + (n*modulus)|n<-0..]]




-- Opdrachten 3 Func. Prog.

--1a: differentieren
----- f = functie  -> is een functie die een double als input neemt (x prob), en een double als output geeft (y prob)
----- p = precizie
----- x = op punt x 
--Differentieer d.m.v. differentiequotiënt

differentieer::(Double->Double)->Double->Double->Double
differentieer f p x = (f(x + p) - f(x)) / p

-- https://wiki.haskell.org/Functional_differentiation
-- in prelude:
-- let func a = a^2 + 6*a + 1 
-- differentieer func (1/10000) 3
            
-- differentiateTest::Double->Double->Double
-- differentiateTest p x =
--     let func a = a^2 + 6*a + 1
--         fx = func x
--         fxp = func (x+p)
--     in ( fxp - fx ) / ( (x+p)-x )




-- Integreer d.m.v. riemannintegratie
-- Deze functie integreert de functie f op het interval a, b met een precisie p.
integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p =
    foldr (\l r->(dx * (f(a + (l * dx)))) + r) 0 [0..p-1]
    where dx = (b-a)/p

 --Scan een lijst voor dubbele entries
dubbelen::(Ord a) => [a]->[a]
dubbelen s = sort(nub((s) \\ (nub s)))

---3: poker (hogere orde functies)


--Stenen
s = [1..6]
stenen = [[a,b,c,d,e]|a<-s,b<-s,c<-s,d<-s,e<-s]
totaalstenen = length (stenen)       


-- Onderstaande functie retourneert het aantal voorkomens van c in een lijst:
count::Integer->[Integer]->Integer
count c [] = 0                  -- lege lijst
count c (x:xs)                  
    |c==x = 1 + (count c xs)    --als huidige element in lijst c is (wat we zoeken), 1 +, and door de rest van de lijst tellen
    |otherwise = count c xs     --anders blijft count hetzelfde
               
        
--Onderstaande functie converteert een lijst in een aantal tuples met voorkomens:
convert list = ([a,b,c,d,e,f],list) 
    where
        a = count 1 list
        b = count 2 list
        c = count 3 list
        d = count 4 list
        e = count 5 list
        f = count 6 list



zelfde::Integer->[[Integer]]->[[Integer]]
zelfde x list = filter (elem x) (map fst (map convert stenen))



poker = (fromIntegral (numberPoker) ) / (fromIntegral (totaalstenen) )
    where
        numberPoker = length (listPoker)
        listPoker = zelfde 5 stenen


fourOfAKind = (fromIntegral (numberFourOfAKind) ) / (fromIntegral (totaalstenen) )
    where
        numberFourOfAKind = length(listFourOfAKind)
        listFourOfAKind = zelfde 4 stenen


fullHouse = (fromIntegral (numberFullHouse) ) / (fromIntegral (totaalstenen) )
    where
        numberFullHouse = length(listFullHouse)
        listFullHouse = filter (elem 2) listThree       --three of a kind + two of a kind
        listThree = zelfde 3 stenen

--(exclusief fullhouses)
threeOfAKind = chanceThree - fullHouse
    where 
        chanceThree = (fromIntegral (numberThree) ) / (fromIntegral (totaalstenen) )
        numberThree = length(listThree)
        listThree = zelfde 3 stenen


twoPairs = (fromIntegral (numberTwoPairs) ) / (fromIntegral (totaalstenen) )
    where
        numberTwoPairs = length(listTwoPairs)
        listTwoPairs = filter (elem 2) (map fst (map convert listPairs))        
        listPairs = zelfde 2 stenen

-- filter de twopairs en fullhouses eruit
pair = chancePair - twoPairs - fullHouse
    where
        chancePair = (fromIntegral (numberPairs) ) / (fromIntegral (totaalstenen) )
        numberPairs = length(listPairs)
        listPairs = zelfde 2 stenen

straight = (fromIntegral (numberStraight) ) / (fromIntegral (totaalstenen) )
    where 
    numberStraight = length(listStraight)
    listStraight = listFiveOnes
    listFiveOnes = filter (elem 5) (map fst (map convert listOnes))     -- 5 stenen met ander resultaat, maar controleert niet de volgorde
    listOnes = zelfde 1 stenen

-- wat overblijft
bust = 1 - poker - fourOfAKind - fullHouse - threeOfAKind - twoPairs - pair - straight

main = do
    let file = "data.txt"
    contents <- readFile file
    putStrLn contents
    let filecomp = "compressed.txt"
    writeFile filecomp "what"
    contentsC <- readFile filecomp
    putStrLn contentsC
	
	length :: ByteString -> Int

