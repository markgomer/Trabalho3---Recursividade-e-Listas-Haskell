--1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell.
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme apresentado aqui, utilizando Haskell.
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b 
  | a >= b = mdc b (a`mod`b)
  | otherwise = mdc a (b`mod`a)

--3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  Utilizando  Haskell  e recursividade.
recursiva :: Int -> Int
recursiva 0 = 0 
recursiva num = (num `mod` 10) + recursiva (num `div` 10)

--4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  que  10000  que sejam múltiplos de 3 ou 5.
somadiv3ou5 :: Int -> Int
somadiv3ou5 0 = 0
somadiv3ou5 x = 
  (if x`mod`3==0 || x`mod`5==0 then x else 0) + somadiv3ou5 (x-1)

--5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando recursividade. 
difs :: [Int] -> Int 
difs x = sq x - (soma x)^2
  where
    sq :: [Int] -> Int
    sq [] = 0
    sq (h:t) = h^2 + sq t
    soma :: [Int] -> Int
    soma [] = 0
    soma (h:t) = h + soma t

--6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos os números primos menores que um determinado inteiro dado. 
ehPrimo :: Int -> Bool
ehPrimo x = if (divisores x == [1,x]) then True else False

divisores :: Int -> [Int]
divisores x = [i | i <- [1..x], x`mod`i==0]

primos :: Int -> [Int]
primos limit = [i | i <- [1..limit], ehPrimo i]


--7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 18, 29, 47, 76, 123) menores que um inteiro dado. 
{-
-}
lucas :: Int -> Int
lucas 0 = 2
lucas 1 = 1
lucas n = lucas (n - 1) + lucas (n - 2)

montaLista :: Int -> [Int]
montaLista limit = [(lucas i) | i <- [0..limit], lucas i < limit]



--8.Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado [1,2,3] devolva [3,2,1]. 
aoContrario :: [Int] -> [Int]
aoContrario [] = []
aoContrario (h:t) = aoContrario t ++ [h]


--9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e devolve o produto destes valores sem usar o operador de multiplicação. 
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x 1 = x
somaRecursiva x n = x + somaRecursiva x (n-1) 


--10. Escreva uma função chamada comprimento que receba uma lista de  inteiros e devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que já calcule o comprimento de uma lista. 
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (h:t) = 1 + comprimento t


main = do  
  putStr "\nFunc. 1: entrada:3; resultado:"
  print (fibonacci 3)

  putStr "Func. 2: entrada:15 10; resultado:"
  print (mdc 15 10)
  putStr "Func. 2: entrada:49 28; resultado:"
  print (mdc 49 28)
  putStr "Func. 2: entrada:21 27; resultado:"
  print (mdc 21 27)

  putStr "Func. 3: entrada:1234; resultado:"
  print (recursiva 1234)
  putStr "Func. 3: entrada:55555; resultado:"
  print (recursiva 55555)
  putStr "Func. 3: entrada:99; resultado:"
  print (recursiva 99)

  putStr "Func. 4: entrada:10000; resultado:"
  print (somadiv3ou5 10000)

  putStr "Func. 5: entrada:[3,5]; resultado:"
  print(difs [3,5])
  putStr "Func. 5: entrada:[3,4]; resultado:"
  print(difs [3,4])
  putStr "Func. 5: entrada:[2,3]; resultado:"
  print(difs [2,3])
  
  putStr "Func. 6: entrada:100; resultado:"
  print(primos 100)



  putStr "Func. 7: entrada:5; resultado:"
  print(montaLista 5)
  putStr "Func. 7: entrada:10; resultado:"
  print(montaLista 10)
  putStr "Func. 7: entrada:20; resultado:"
  print(montaLista 20)
  putStr "Func. 7: entrada:30; resultado:"
  print(montaLista 30)

  putStr "Func. 8: entrada:[1,2,3]; resultado:"
  print(aoContrario [1,2,3])
  putStr "Func. 8: entrada:[5,4,3,2,1]; resultado:"
  print(aoContrario [5,4,3,2,1])
  
  putStr "Func. 9: entrada:4,5; resultado:"
  print(somaRecursiva 4 5)
  putStr "Func. 9: entrada:8,7; resultado:"
  print(somaRecursiva 8 7)
  
  putStr "Func. 10: entrada:[1,3,5,5,23,26,23,62,72,7]; resultado:"
  print(comprimento [1,3,5,5,23,26,23,62,72,7])