module Exercice2Half where

type Var = String

-- Expressions du code source en forme ASA.
data Exp = 
    Enum Int                -- Une constante
    | Evar Var              -- Une variable
    -- | Elet Var Exp Exp   -- "let X = E1 in E2"
    | Ecall Exp Exp         -- Un appel de fonction
    deriving (Show) 

-- Valeurs renvoyees.
data Val = Vnum Int         -- Un nombre entier
    | Vprim (Val -> Val)    -- Une primitive

elookup x ((x1,v1):env) =
    if x == x1 then v1 else elookup x env

eval env (Enum n) = Vnum n

eval env (Evar x) = elookup env [(0, x)]

-- eval env (Elet x e1 e2) =   
--     let v = eval env e1 in eval ((x,v):env) e2

-- eval env (Ecall fun actual) = 
    -- case eval env fun of Vprim f -> f (eval actual)
