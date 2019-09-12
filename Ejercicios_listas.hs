module Ejercicios_Listas where

mayor::[Int]->Int
mayor [x] = x
mayor (x:xs) | x > mayor(xs) = x | otherwise = mayor(xs)

mayores:: [[Int]]->[Int]
mayores []=[]
mayores lista= [mayor(x) | x <- lista]

sumar::[Int]->Int
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

menor::[Int]->Int
menor [x] = x
menor (x:xs) | x < menor(xs) = x | otherwise = menor(xs)

promedio::[Int]->Int
promedio lista = div (sumar lista) (length lista)

promedios::[[Int]]->[Int]
promedios lista = [promedio x | x <- lista]

mascercano:: [Int]->Int->Int
mascercano [a] b = a
mascercano (x:xs) b | abs (x - b) < (mascercano (xs) b) = x | otherwise = (mascercano xs b)
