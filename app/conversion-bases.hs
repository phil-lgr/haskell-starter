
-- Conversion de base 10 à base 2
-- Exemple  dec2bin 17 => 10001

dec2bin :: Int -> Int
dec2bin 0 = 0
dec2bin x =
  bit + 10 * dec2bin next
  where
    bit  = x `mod` 2
    next = x `div` 2

-- Conversion de base 2 à base 10
-- Exemple  bin2dec 10001 => 17

bin2dec :: Int -> Int
bin2dec 0 = 0
bin2dec x =
  let bit  = x `mod` 2
      next = x `div` 10
  in bit + 2 * bin2dec next


-- J'ai utilisé where dans dec2bin, et let in dans bin2dec
-- pour vous montrer qu'ils sont interchangeables


-- Conversion de base X à base Y
-- L'idée est d'utiliser la base 10 comme intermédiaire
baseconv :: Int -> Int -> Int -> Int
baseconv oldbase newbase x =
  dec2any newbase (toDec oldbase x)
  where
    toDec :: Int -> Int -> Int
    toDec _ 0 = 0
    toDec base x =
      let bit  = x `mod` 10
          next = x `div` 10
      in bit + base * toDec base next

    dec2any :: Int -> Int -> Int
    dec2any _ 0 = 0
    dec2any base x =
      let bit  = x `mod` base
          next = x `div` base
      in bit + 10 * dec2any base next


-- On peut ensuite faire
bin2dec' = 2  `baseconv` 10
dec2bin' = 10 `baseconv` 2


