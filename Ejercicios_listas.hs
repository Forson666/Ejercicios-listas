module Ejercicios_Listas where

mayor::[Int]->Int
mayor [x] = x
mayor (x:xs) | x > mayor(xs) = x | otherwise = mayor(xs)

mayores:: [[Int]]->[Int]
mayores []=[]
mayores lista= [mayor(x) | x <- lista]

menor::[Int]->Int
menor [x] = x
menor (x:xs) | x < menor(xs) = x | otherwise = menor(xs)

sumar::(Ord a, Fractional a) =>[a]->a
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

promedio :: (Ord a, Fractional a) => [a] -> a
promedio [] = 0
promedio xs = sumar xs / fromIntegral (length xs)

promedios :: (Ord a, Fractional a) => [[a]] -> [a]
promedios lista = [promedio x | x <- lista]

mascercano:: (Ord a, Fractional a) => [a]->a->a
mascercano [a] b = a
mascercano (x:xs) b | abs (x - b) < abs (mascercano (xs) b - b) = x | otherwise = mascercano xs b

mascercanos:: (Ord a, Fractional a) => [[a]]->[a]
mascercanos []=[]
mascercanos lista= [mascercano x (promedio x) | x <- lista]

divisible::Int->Int->Bool
divisible x y = (mod x y) ==0

cant_multiplos::[Int]->Int->Int
cant_multiplos [x] a | (mod x a) == 0  && x /= a = 1 | otherwise = 0
cant_multiplos (x:xs) a | (mod x a) == 0  && x /= a = 1 + cant_multiplos xs a | otherwise = cant_multiplos xs a

cant_divisores::[Int]->Int->Int
cant_divisores [x] a | (mod a x) == 0  && x /= a = 1 | otherwise = 0
cant_divisores (x:xs) a | (mod a x) == 0  && x /= a = 1 + cant_divisores xs a | otherwise = cant_divisores xs a

hallarTupla::[[Int]]->[(Int,Int,Int,Int)]
hallarTupla lista = [(menor x ,mayor x ,cant_multiplos x (menor x) , cant_divisores x (mayor x))| x <- lista]
