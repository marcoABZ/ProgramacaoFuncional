-- Marco Antonio Barbosa Zulian
{-- 1. Escreva  uma  função  para  o  cálculo  dos  números  da  sequência  de  Fibonacci,  utilizando Haskell.  --} 
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
seqFibonacci :: Int -> [Int]
seqFibonacci 0 = []
seqFibonacci n = [fibonacci x | x <- [1..n]]

{-- 2. Um dos primeiros algoritmos documentados é o algoritmo para o cálculo do Maior 
Divisor Comum  (MDC)  de  Euclides  publicado  por  volta do  ano 300  AC.  Podemos 
simplificar  este algoritmo  dizendo  que  dados  dois  inteiros  A  e  B,  o  MDC  
entre  eles  será dado  pelo  valor absoluto de A se B=0 e pelo MDC entre B e o resto 
da divisão de A por B se B>0. Escreva uma  função  para  o  cálculo  do  MDC  entre  
dois  números  inteiros  positivos,  usando  o algoritmo de Euclides conforme 
apresentado aqui, utilizando Haskell. --}
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

{-- 3. Escreva uma função recursiva que dado um número inteiro n, devolva a soma dos 
dígitos deste  número.  Exemplo:  dado  1234  a  função  deverá  devolver  10.  
Utilizando  Haskell  e recursividade. --}
somaDigitos :: Int -> Int
somaDigitos x
  | x < 10 && x >= 0 = x
  | x < 0 = -somaDigitos(-x)
  | otherwise = x `mod` 10 + somaDigitos (x `quot` 10)

{-- 4. Escreva  uma  função  que  devolva  a  soma  de  todos  os  números  menores  
que  10000  que sejam múltiplos de 3 ou 5. --}
multiplos35 = [x | x <- [1..10000], x `mod` 3 == 0 || x `mod` 5 == 0]

{-- 5. Escreva  uma  função que,  recebendo  uma  lista  de  inteiros,  apresente  a  
diferença  entre a soma dos quadrados e o quadrado da soma destes inteiros, usando 
recursividade. --}
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x: xs) = x + somaLista xs

quadradoLista :: [Int] -> [Int]
quadradoLista xs = [x^2 | x<-xs] 

diferencaSomaQuadQuadSoma :: [Int] -> Int
diferencaSomaQuadQuadSoma x =
  let
    somaQuad = somaLista (quadradoLista x)
    quadSoma = (somaLista x)^2
  in somaQuad - quadSoma

{--6. O Crivo de Eratóstenes não é o melhor algoritmo para encontrar números primos. 
Crie uma função que implemente o Crivo de Euler (Euler’s Sieve) para encontrar todos 
os números primos menores que um determinado inteiro dado.  --}
passoCrivo :: Int -> [Int] -> [Int]
passoCrivo limite [] = []
passoCrivo limite lista
  | head lista > limite = lista
  | otherwise = [head lista] ++ passoCrivo limite ([x | x <- tail lista, x `mod` (head lista) /= 0])

crivo :: Int -> [Int]
crivo x = passoCrivo x [2..x]

{--7. Nem  só  de  Fibonacci  vivem  os  exemplos  de  recursão.  Escreva  uma  
função  que  devolva todos os números de uma sequência de Lucas (2, 1, 3, 4, 7, 11, 
18, 29, 47, 76, 123) menores que um inteiro dado. --} 
menoresQueXLucas :: Int -> Int -> Int -> [Int] -> [Int]
menoresQueXLucas penultimo ultimo limite elementos
  | limite > ultimo + penultimo = menoresQueXLucas ultimo (penultimo + ultimo) limite (elementos ++ [ultimo + penultimo])
  | otherwise = elementos

lucas :: Int -> [Int]
lucas 1 = [1]
lucas 2 = [2]
lucas x = if x < 1 then [] else menoresQueXLucas 2 1 x [2, 1]

{--8. Escreva uma função, chamada aoContrario em Haskel para reverter uma lista. Dado 
[1,2,3] devolva [3,2,1]. --}
-- CASO BASE NÃO FUNCIONA
-- ARRAY DE CHAR
aoContrario :: [a] -> [a]
aoContrario [] = []
aoContrario x = [last x] ++ aoContrario (init x)

{--9. Escreva uma função chamada somaRecursiva que recebe dois valores inteiros e 
devolve o produto destes valores sem usar o operador de multiplicação. --}
somaRecursiva :: Int -> Int -> Int
somaRecursiva x 0 = 0
somaRecursiva x y = if y < 0 then -1 * (x + somaRecursiva x (-y-1)) else (x + somaRecursiva x (y-1))

{--10. Escreva uma função chamada comprimento que receba uma lista de  inteiros 
devolva o comprimento desta lista. Observe que você não pode usar nenhuma função que
já calcule o comprimento de uma lista. --}
comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

main = do
  putStrLn ("Func. 1: entrada: 0; resultado: " ++ show(seqFibonacci 0))
  putStrLn ("Func. 1: entrada: 5; resultado: " ++ show(seqFibonacci 5))
  putStrLn ("Func. 2: entrada: 23732 180; resultado: " ++ show(mdc 23732 180))
  putStrLn ("Func. 3: entrada: 1234; resultado: " ++ show(somaDigitos 1234))
  putStrLn ("Func. 3: entrada: 0; resultado: " ++ show(somaDigitos 0))
  putStrLn ("Func. 3: entrada: -3579; resultado: " ++ show(somaDigitos (-3579)))
  putStrLn ("Teste func 4 ao fim do arquivo, por conta do tamanho do output.")
  putStrLn ("Func. 5: entrada: [1,2,-3]; resultado: " ++ show(diferencaSomaQuadQuadSoma [1,2,(-3)]))
  putStrLn ("Func. 5: entrada: [1,2,3]; resultado: " ++ show(diferencaSomaQuadQuadSoma [1,2,3]))
  putStrLn ("Func. 6: entrada: -20; resultado: " ++ show(crivo (-20)))
  putStrLn ("Func. 6: entrada: 0; resultado: " ++ show(crivo (0)))
  putStrLn ("Func. 6: entrada: 20; resultado: " ++ show(crivo (20)))
  putStrLn ("Func. 7: entrada: -5; resultado: " ++ show(lucas (-5)))
  putStrLn ("Func. 7: entrada: 1; resultado: " ++ show(lucas 1))
  putStrLn ("Func. 7: entrada: 2; resultado: " ++ show(lucas 2))
  putStrLn ("Func. 7: entrada: 11; resultado: " ++ show(lucas 11))
  putStrLn ("Func. 8: entrada: [1.3,-2.5,3]; resultado: " ++ show(aoContrario [1.3,(-2.5),3]))
  putStrLn ("Func. 8: entrada: [1,2,3]; resultado: " ++ show(aoContrario [1,2,3]))
  putStrLn ("Func. 8: entrada: [a,b,c]; resultado: " ++ show(aoContrario ['a','b','c']))
  putStrLn ("Func. 9: entrada: 2 0; resultado: " ++ show(somaRecursiva 2 0))
  putStrLn ("Func. 9: entrada: 5 3; resultado: " ++ show(somaRecursiva 5 3))
  putStrLn ("Func. 9: entrada: 4 -2; resultado: " ++ show(somaRecursiva 4 (-2)))
  putStrLn ("Func. 10: entrada: []; resultado: " ++ show(comprimento []))
  putStrLn ("Func. 10: entrada: [1]; resultado: " ++ show(comprimento [1]))
  putStrLn ("Func. 10: entrada: [1,2,3]; resultado: " ++ show(comprimento [1,2,3]))

  putStrLn ("Func. 4: entrada: ; resultado: " ++ show(multiplos35))