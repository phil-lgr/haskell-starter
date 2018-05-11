
data Exp = Enum Int         -- Une constante
         | Eplus
         | Etimes
         | Eneg
         | Egt
         | Enot
         | Ecall Exp [Exp]  -- Ecall fonction [arguments]
         | Eif Exp Exp Exp  -- if e1 then e2 else e3
         deriving Show

data Val = Vnum Int
         | Vbool Bool
         deriving Show

eval :: Exp -> Val

eval e = case e of

  Enum n -> Vnum n

  Ecall Eplus [e1, e2] ->
    Vnum (e1' + e2')
    where
      Vnum e1' = eval e1
      Vnum e2' = eval e2

  Ecall Etimes [e1, e2] ->
    Vnum (e1' * e2')
    where
      Vnum e1' = eval e1
      Vnum e2' = eval e2

  --Ecall Eneg ... ->
  --  ...

  --Eif e1 e2 e3 ->
  --  ...


eplus  e1 e2 = Ecall Eplus  [e1, e2]
etimes e1 e2 = Ecall Etimes [e1, e2]
eneg   e     = Ecall Eneg   [e]
egt    e1 e2 = Ecall Egt    [e1, e2]
enot   e     = Ecall Enot   [e]







