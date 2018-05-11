
data Exp = Enum Int       -- Une constante
         | Eplus Exp Exp  -- e1 + e2
         | Etimes Exp Exp -- e1 * e2
         deriving (Eq)

optimize :: Exp -> Exp

-- L'idée est de faire en premier la récursivité, et en second l'optimisation

optimize e = case e of

  Enum a -> Enum a

  Eplus a b
    | a' == Enum 0  -> b'
    | b' == Enum 0  -> a'
    | otherwise     -> Eplus a' b'
    where
      a' = optimize a
      b' = optimize b

  Etimes a b
    | a' == Enum 0  -> Enum 0
    | b' == Enum 0  -> Enum 0
    | a' == Enum 1  -> b'
    | b' == Enum 1  -> a'
    | otherwise     -> Etimes a' b'
    where
      a' = optimize a
      b' = optimize b

-- Essayons de simplifier l'expression
-- 1 * (1 + 0 + 9) * 1 * 1 + 0 + 0 * (0 * 0 + 2)
-- C'est-à-dire :
-- Eplus (Eplus (Etimes (Etimes (Etimes (Enum 1) (Eplus (Eplus (Enum 1) (Enum 0)) (Enum 9))) (Enum 1)) (Enum 1)) (Enum 0)) (Etimes (Enum 0) (Eplus (Etimes (Enum 0) (Enum 0)) (Enum 2)))
-- On va obtenir Eplus (Enum 1) (Enum 9)
-- Donc 1 + 9
-- Ce qui est exact
