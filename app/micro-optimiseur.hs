
data Exp = Enum Int       -- Une constante
         | Eplus Exp Exp  -- e1 + e2
         | Etimes Exp Exp -- e1 * e2
         deriving Show

optimize :: Exp -> Exp

optimize (Enum a)            = Enum a

optimize (Eplus (Enum 0) b)  = optimize b
optimize (Eplus a (Enum 0))  = optimize a

optimize (Etimes (Enum 0) _) = Enum 0
optimize (Etimes _ (Enum 0)) = Enum 0

optimize (Etimes (Enum 1) b) = optimize b
optimize (Etimes a (Enum 1)) = optimize a

optimize (Eplus a b)         = Eplus (optimize a) (optimize b)
optimize (Etimes a b)        = Etimes (optimize a) (optimize b)
-- Sur ces deux dernières lignes, on peut se demander :
-- et si (optimize a) nous donne un Enum 0 finalement ?
-- alors ce ne sera pas completement optimisé ...
-- Ce micro-optimiseur ne fait "qu'une passe" d'optimisation
-- Vous pouvez regarder 'micro-optimiseur-v2.hs' qui regle ce petit problème

-- Même version qu'au dessus mais avec case of

optimize' :: Exp -> Exp

optimize' e = case e of

  Enum a            -> Enum a

  Eplus (Enum 0) b  -> optimize' b
  Eplus a (Enum 0)  -> optimize' a

  Etimes (Enum 0) _ -> Enum 0
  Etimes _ (Enum 0) -> Enum 0

  Etimes (Enum 1) b -> optimize' b
  Etimes a (Enum 1) -> optimize' a

  Eplus a b         -> Eplus (optimize' a) (optimize' b)
  Etimes a b        -> Etimes (optimize' a) (optimize' b)


