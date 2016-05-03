import Data.Char
import Data.List.Split
import Control.Exception


separa::[Char]->[Char]->[[Char]]
separa [] (x) = [(x)]
separa (x:xs) (y) = if x==';' then [(y)]++separa xs [] else separa xs ((y)++[x])

escr::[[Char]]->[[Char]]->Int->[Char]
escr (x:xs) (y:ys) 0 = x++" <- "++escr xs ys 1
escr (x:xs) [] n = []
escr [] _ n = []
escr (x:xs) (y:ys) n = if x==[] then escr xs ys (n+1) else escr2 x y++escr xs ys (n+1)

reverte::[Char]->[Char]
reverte [] = []
reverte (x:xs) = reverte xs++[x]

tiraN::[Char]->Int->[Char]
tiraN [] n = []
tiraN (x:xs) 0 = (x:xs)
tiraN (x:xs) n = tiraN xs (n-1)

limpa::[Char]->Int->[Char]
limpa [] n = []
limpa (x:xs) n = do
 let y = reverte (x:xs)
 let v = tiraN y n
 reverte v

size::[a]->Int
size [] = 0
size (x:xs) = 1+size xs
 
ler::[[Char]]->Int->IO()
ler [] n = putStr ""
ler (x:xs) 0 = putStr ""
ler (x:xs) n = putStrLn (limpa ((escr (separa ((x:xs)!!n) []) (separa ((x:xs)!!0) []) 0)) 3)

ler2 (x:xs) n m = if n==m then putStr "" else do 
 ler (x:xs) n
 ler2 (x:xs) (n+1) m 

escr2::[Char]->[Char]->[Char]
escr2 [] (y) =" "++(y)++" | "
escr2 (x:xs) (y) = if x==',' then " "++(y)++" | "++escr2 xs (y) else [x]++escr2 xs (y)

initial::[[Char]]->IO()
initial (x:xs) = putStrLn ("Estado inicial: "++x)

final::[Char]->Int->Int
final [] n = n
final (x:xs) n = if x=='E' then n else if x/=';' then final xs (n+1) else final xs n

final2 (x:xs) n m l = if n==m then putStr "" else do
 final3 ((x:xs)!!n) l
 final2 (x:xs) (n+1) m l

final3::[Char]->Int->IO()
final3 (x:xs) l = do
 result <- try (evaluate ((separa (x:xs) [])!!l)) :: IO (Either SomeException [Char])
 case result of
  Left ex  -> putStr $ ""
  Right val -> putStr $ val++" "

separaArq::[Char]->[[Char]]
separaArq [] = []
separaArq (x:xs) = splitOn "\n" (x:xs)

main = do
 putStrLn "Digite o nome do arquivo"
 y <- getLine
 x <- readFile y
 let n = separaArq x
 let v = size n
 ler2 n 1 v
 initial (separa ((n!!1)) [])
 putStr "Estados Finais: "
 final2 n 1 v (final (n!!0) 1)
 putStrLn ""
 j <- getLine
 putStrLn ""

 
 

 