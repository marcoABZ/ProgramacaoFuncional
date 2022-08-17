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

-- Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟∷(𝐼𝑛𝑡,𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
somaPar :: (Int,Int) -> Int
somaPar (x, y) = x + y

-- 7. Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥^2 +𝑦/2 + 𝑧. 
equacao :: Double -> Double -> Double -> Double
equacao x y z = x**2 + y / 2 + z

-- 8. Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagnóstico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  mórbida:  entenda  a  diferença  entre  os  três  termos (cuidadospelavida.com.br).  Observe  que  este  diagnóstico  é  meramente  estatístico  e  não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico.

-- 9. Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
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
  print(soma1 3)
  print(sempre 'a')
  print(sempre 1.3)
  print(sempre (-4))
  print(soma1 (-5))
  print(treco 1 2 3)
  print(resto 25 2)
  print(precoMaior 1.0 7.0 13.0 (9.0))
  print(impar 2 5)
  print(equacao 2 6 3)
  print(bissexto 1600)
  print(somaPar (10, 20))