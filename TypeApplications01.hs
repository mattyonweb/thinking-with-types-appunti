{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{- Esempi di Type Application (sostituzione di tipi
polimorfici di una funzione con tipi concreti, mediante
notazione @Type) e scrittura di una strana funzione
per stampare su stringa un tipo. -}

import Data.Typeable

{-
fmap :: Functor f => (a -> b) -> f a -> f b

Specializzo fmap sfruttando -XTypeApplications
e notazione @Type.
-}

-- (a -> b) -> Maybe a -> Maybe b
maybeFmap = fmap @Maybe

-- ([String] -> b) -> Maybe [String] -> Maybe b
maybeFmapInt = fmap @Maybe @[String]

{- Questo non compila perché si lamenta di type variables ambigue;
tuttavia, lanciandolo da REPL, è un'espressione ben tipata e
ritorna correttamente il tipo:
    fmap @_ @Int @String
         :: Functor w => (Int -> String) -> w Int -> w String
-}
-- genericFunctorFmap = fmap @_ @Int @String

---------------------------------------------------

{- Questa funzione è abbastanza anomala: converte in stringa
il nome di un tipo. Il problema è che la funzione non riceve
nessun argomento! Come funziona quindi?

TypeApplications -> serve per la sintassi @a.
ScopedTypeVariables -> serve per dare uno scope ai tipi dichiarati.
    Significa che, avendo una signatura tipo forall a. (...),
    posso referenziare lo stesso "a" anche nel corpo della funzione.
AllowAmbiguousTypes -> serve perché "Typeable a" rimane ambiguo in
    fase di compilazione: a che diavolo corrisponde "a"? 
-}

typeName :: forall a. Typeable a => String
typeName = show $ typeRep (Proxy @a)

es1 = typeName @Int
es2 = typeName @(Maybe Int)
es3 = typeName @(Either [String] (Maybe Int))
