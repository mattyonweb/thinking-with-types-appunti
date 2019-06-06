{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

{- Esempi preliminari su Constraints. (la parte che sta a
sinistra della frecciona "=>") -}

cinque :: Int
cinque = 5

-- (a ~ Int) è un eguaglianza tra tipi, aka. Type Equality.
-- (~) == (==)
cinqueInutilmenteElaborato :: (a ~ Int) => a
cinqueInutilmenteElaborato = 5


-- Esempio di AST Syntax senza GADT: nota che esistono delle
-- espressioni ammesse da questa sintassi che però sono
-- semanticamente scorrette (es. NaiveNot (NaiveLitInt 5) è un'
-- operazione senza senso, NaiveNot dovrebbe operare solo
-- su booleani!)
data NaiveE t
  = NaiveLitInt Int
  | NaiveLitBool Bool
  | NaiveAdd (NaiveE Int) (NaiveE Int)
  | NaiveNot (NaiveE Bool)
  | NaiveIf (NaiveE Bool) (NaiveE t) (NaiveE t)

shouldNotBePossible1 =
  NaiveAdd (NaiveLitBool True) (NaiveLitBool False)
shouldNotBePossible2 =
  NaiveNot $ NaiveLitInt 5


-- Esempio di definizione di tipo via sintassi GADT: ora le
-- espressioni possibili in questo linguaggio sono le sole
-- e uniche espressioni sensate! Eventuali espressioni senza
-- senso vengono intercettare type-checking-time.
data Expr t where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

{-
  notPossible1 = Not (LitInt 4)

/home/groucho/interscambio/haskell/type-programming/Constraints01.hs:49:21-28: error: …
    • Couldn't match type ‘Int’ with ‘Bool’
      Expected type: Expr Bool
        Actual type: Expr Int
-}

-- Nota che scrivere l'eval è velocissimo: avendo eliminato tutti i possibili
-- errori di tipo via type-system, non devo controllare per es. che in
-- (If c x y) c sia di tipo Bool, x e y siano dello stesso tipo ecc... tutto
-- viene fatto a monte e gratuitamente da Haskell!
eval :: Expr t -> t
eval (LitInt x)  = x
eval (LitBool b) = b
eval (Add x y)   = (eval x) + (eval y)
eval (Not x)     = not $ eval x
eval (If c x y)  = if (eval c) then (eval x) else (eval y)

expr1 = eval $ Add (LitInt 5) (Add (LitInt (-3)) (LitInt (-2))) -- 0

{- Di fatto GADT fornisce dello zucchero sintattico per definire Type Equalities.
La seguente definizione di Expr_ a è equivalente a Expr t:

data Expr_ a
  = (a ∼ Int) => LitInt_ Int
  | (a ∼ Bool) => LitBool_ Bool
  | (a ∼ Int) => Add_ (Expr_ Int) (Expr_ Int)
  | (a ∼ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

Purtroppo per qualche motivo questa nuova versione non compila perché:

/home/groucho/interscambio/haskell/type-programming/GADT01.hs:75:8: error: …
    Not in scope: type constructor or class ‘∼’

In effetti: che sintassi è

(a ~ Int) => LitInt_ Int

? Cioè capisco ad alto livello cosa si vuole fare ma _perché_ dovrebbe funzionare?

-}
