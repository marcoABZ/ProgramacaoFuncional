-- Marco Antônio Barbosa Zulian
-- 1. Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.  
soma1 :: Int -> Int
soma1 n = n + 1

-- 2. Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo.
sempre :: a -> Int
sempre x = 0

-- 3. Escreva  uma  função  chamada  treco  que  receba  três  valores  em  ponto  flutuantes  com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro.
treco :: Double -> Double -> Double -> Double
treco x y z = (x + y) * z

-- 4. Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros. 
resto :: Int -> Int -> Int
resto n m = n `mod` m

-- 5. Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.
precoMaior :: Float -> Float -> Float -> Float -> Float
precoMaior a b c d 
  | a >= b && a >= c && a >= d = a
  | b >= a && b >= c && b >= d = b
  | c >= a && c >= b && c >= d = c
  | otherwise = d

-- 6. Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.  
impar :: Int -> Int -> Bool
impar x y = (x * y) `mod` 2 == 1

-- 7. Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
somaPar :: (Int,Int) -> Int
somaPar (x, y) = x + y

-- 8. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥^2 +𝑦/2 + 𝑧. 
equacao :: Double -> Double -> Double -> Double
equacao x y z = x**2 + y / 2 + z

-- 9. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.
diagnostico :: Float -> Float -> String
diagnostico x y
  | x / (y * y) < 17 = "Muito abaixo do peso"
  | x / (y * y) < 18.49 = "Abaixo do peso"
  | x / (y * y) < 24.99 = "Peso normal"
  | x / (y * y) < 29.99 = "Sobrepeso"
  | x / (y * y) < 34.99 = "Obesidade leve"
  | x / (y * y) < 39.99 = "Obesidade severa"
  | otherwise = "Obesidade morbida"

-- 10. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
-- 𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 
--      𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 
--            𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 
-- 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto.
bissexto :: Int -> Bool
bissexto x
  | x `mod` 400 == 0 = True
  | x `mod` 100 == 0 = False
  | x `mod` 4 == 0 = True
  | otherwise = False

main = do

  putStrLn ("Func. 1: entrada: 2; resultado: " ++ show(soma1 2))
  putStrLn ("Func. 1: entrada: -1; resultado: " ++ show(soma1 (-1)))
  putStrLn ("Func. 2: entrada: 'a'; resultado: " ++ show(sempre 'a'))
  putStrLn ("Func. 2: entrada: 1.3; resultado: " ++ show(sempre 1.3))
  putStrLn ("Func. 2: entrada: -4; resultado: " ++ show(sempre (-4)))
  putStrLn ("Func. 3: entrada: 1 2 3; resultado: " ++ show(treco 1 2 3))
  putStrLn ("Func. 3: entrada: -5 2 1; resultado: " ++ show(treco (-5) 2 1))
  putStrLn ("Func. 3: entrada: -5 -2 -3; resultado: " ++ show(treco (-5) (-2) (-3)))
  putStrLn ("Func. 4: entrada: 25 2; resultado: " ++ show(resto 25 2))
  putStrLn ("Func. 4: entrada: -4 2; resultado: " ++ show(resto (-4) 2))
  putStrLn ("Func. 4: entrada: -5 2; resultado: " ++ show(resto (-5) 2))
  putStrLn ("Func. 5: entrada: 10 9 -8 -3; resultado: " ++ show(precoMaior 10 9 (-8) (-3)))
  putStrLn ("Func. 5: entrada: 3 9 -8 -3; resultado: " ++ show(precoMaior 3 9 (-8) (-3)))
  putStrLn ("Func. 5: entrada: -10 -9 -8 -11; resultado: " ++ show(precoMaior (-10) (-9) (-8) (-11)))
  putStrLn ("Func. 5: entrada: 10 9 -8 25; resultado: " ++ show(precoMaior 10 9 (-8) 25))
  putStrLn ("Func. 6: entrada: 1 2; resultado: " ++ show(impar 1 2))
  putStrLn ("Func. 6: entrada: 4 2; resultado: " ++ show(impar 4 2))
  putStrLn ("Func. 6: entrada: 5 3; resultado: " ++ show(impar 5 3))
  putStrLn ("Func. 6: entrada: -1 2; resultado: " ++ show(impar (-1) 2))
  putStrLn ("Func. 6: entrada: -4 -2; resultado: " ++ show(impar (-4) (-2)))
  putStrLn ("Func. 6: entrada: 5 -3; resultado: " ++ show(impar 5 (-3)))
  putStrLn ("Func. 7: entrada: (10 20); resultado: " ++ show(somaPar (10, 20)))
  putStrLn ("Func. 7: entrada: (-10 3); resultado: " ++ show(somaPar ((-10), 3)))
  putStrLn ("Func. 7: entrada: (-10 -3); resultado: " ++ show(somaPar ((-10), (-3))))
  putStrLn ("Func. 8: entrada: -10 20 -110; resultado: " ++ show(equacao (-10) 20 (-110)))
  putStrLn ("Func. 8: entrada: 5 -17 30; resultado: " ++ show(equacao 5 (-17) 30))
  putStrLn ("Func. 9: entrada: 20.5 1.3; resultado: " ++ show(diagnostico 20.5 1.3))
  putStrLn ("Func. 9: entrada: 40 1.5; resultado: " ++ show(diagnostico 40 1.5))
  putStrLn ("Func. 9: entrada: 90 1.9; resultado: " ++ show(diagnostico 90 1.9))
  putStrLn ("Func. 9: entrada: 100 1.9; resultado: " ++ show(diagnostico 100 1.9)) 
  putStrLn ("Func. 9: entrada: 110 1.9; resultado: " ++ show(diagnostico 110 1.9))
  putStrLn ("Func. 9: entrada: 110 1.7; resultado: " ++ show(diagnostico 110 1.7))
  putStrLn ("Func. 9: entrada: 110 1.3; resultado: " ++ show(diagnostico 110 1.3))
  putStrLn ("Func. 10: entrada: 2000; resultado: " ++ show(bissexto 2000))
  putStrLn ("Func. 10: entrada: 1800; resultado: " ++ show(bissexto 1800))
  putStrLn ("Func. 10: entrada: 1996; resultado: " ++ show(bissexto 1996))
  putStrLn ("Func. 10: entrada: 1997; resultado: " ++ show(bissexto 1997))