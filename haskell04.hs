-- Prática 04 de Haskell
-- Nome: Lucas Caetano 

faixaIdoso :: Int -> String
faixaIdoso x = 
  if 60 <=  x && 64 >= x then "IDO64" 
  else if  65 <= x && x <= 69 then "IDO69"
  else if  70 <= x &&  x <= 74 then "IDO74"
  else if  75 <= x && x <= 79 then "IDO79"
  else if x >= 80 then "IDO80"
  else "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos x = [(fst z,snd z,faixaIdoso(snd z)) | z <- x ]

strColor :: (Int,Int,Int) -> String
strColor x = "rgb" ++ show x

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = take n [(x,cy,r) | x <- (iterate (+2) cx)]

genReds :: Int -> [(Int,Int,Int)]
genReds x = take x [(y,0,0) | y <- [2,2 + truncate (fromIntegral x*1.33)..], y < 255]
