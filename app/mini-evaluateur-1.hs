
data Exp = Enum Int         -- Une constante
         | Eplus Exp Exp    -- e1 + e2
         | Etimes Exp Exp   -- e1 * e2
         | Eneg Exp         -- (- e)
         | Egt Exp Exp      -- e1 > e2
         | Enot Exp         -- (not e)
         | Eif Exp Exp Exp  -- if e1 then e2 else e3
         deriving Show

data Val = Vnum Int
         | Vbool Bool
         deriving Show

eval :: Exp -> Val

-- Les Vnum

eval (Enum v) =
  Vnum v

eval (Eplus a b) =
  Vnum (a' + b')
  where
    Vnum a' = eval a
    Vnum b' = eval b

eval (Etimes a b) =
  Vnum (a' * b')
  where
    Vnum a' = eval a
    Vnum b' = eval b

eval (Eneg a) =
  Vnum (- a')
  where
    Vnum a' = eval a

-- Les Vbool

eval (Egt a b) =
  Vbool (a' > b')
  where
    Vnum a' = eval a
    Vnum b' = eval b

eval (Enot a) =
  Vbool (not a')
  where
    Vbool a' = eval a

eval (Eif p a b) =
  if p' then eval a else eval b
  where
    Vbool p' = eval p

{-

  Essayons d'évaluer : if (10 > 2) then (2 + 4) else 1

  C'est-à-dire, cette expression :

  (Eif (Egt (Enum 10) (Enum 2)) (Eplus (Enum 2) (Enum 4)) (Enum 1))

  Nous obtenons Vnum 6

-}


