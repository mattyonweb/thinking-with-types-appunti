{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

import GHC.TypeLits
import Control.Monad (liftM)

-- Forzare una proprietà (essere Admin/non esserlo) a livello
-- di tipi.

data UserType
  = SimpleUser
  | Admin
  deriving (Show)

{-
SENZA DataKinds, la dichiarazione qui sopra è una dichiarazione di
_tipo_ (UserType) seguita dai costruttori per i _valori_ (SimpleUser, Admin):

K:  *
    ^
    |
Ty: UserType 
       ^   ^---+
       |        \
Tm: SimpleUser  Admin


CON DataKinds la dichiarazione qui sopra, oltre a essere dichiarazione
di tipo/valore, viene anche lift-ata in dichiarazione di kind/tipo:

K:  UserType
       ^  ^-----+
       |         \
Ty: 'SimpleUsr  'Admin 

Tm:          N/A


In pratica così otteniamo tutte queste cose:

+ SimpleUser (:t UserType, :k UserType) costruisce _valori_ di tipo UserType:

> x = SimpleUser
> :t x
x :: UserType

+ 'SimpleUser (:t ERROR, :k UserType) è un _tipo_, con kind UserType. Può essere usato
per dichiarare funzioni con ristrettezze particolari; v. dopo, con Privilege e funzioni
solo per gli Admins.

+ UserType (:k *) è sia il tipo dei valori costruiti con Admin e SimpleUser, ma (credo)
è anche un kind (?)

-}

-- a :: UserType
-- è una KIND-ANNOTATION, significa che
-- Privilege si aspetta un tipo accompagnante che
-- abbia kind UserType

-- Privilege (a :: UserType) è sia:
--   + Un tipo Privilege che prende un altro tipo di tipo a generico
--   + Un kind Privilege che prende un tipo a con kind UserType
data Privilege (a :: UserType) = -- a :: UserType è una kind annotation
  PrivilegeLevel UserType
  deriving (Show)

{-
λ> :k Privilege
Privilege :: UserType -> *

λ> :k Privilege 'Admin
Privilege 'Admin :: *

λ> :k Privilege Admin 
Privilege Admin :: *  -- Hask è intelligente e capisce che qui Admin = 'Admin

λ> :k Privilege 'SimpleUser
Privilege 'SimpleUser :: *

λ> :k Privilege Int
<interactive>:1:11-13: error:
    • Expected kind ‘UserType’, but ‘Int’ has kind ‘*’

`Privilege a` è un tipo che aspetta un altro tipo, `a`.

Questo tipo `a` deve avere un kind UserType, come descritto
nella kind-signature di Privilege.

'Admin (aka. il costruttore Admin di valori elevato a tipo) e 'SimpleUser
sono, in questo caso, gli unici due tipi che Privilege accetta come `a`.
Privilege Int, pertanto, solleva un errore.
-}

data User = User 
  { name :: String
  , privilege :: Maybe (Privilege 'Admin)
  -- v. intestazione: data Privilege 'Admin,
  -- dove Privilege è un tipo con segnatura-kind
  -- Privilege (a :: UserType)
  -- e 'Admin è un tipo con kind UserType
  } deriving (Show)


azionePrivilegiata :: Privilege 'Admin -> String
azionePrivilegiata _ = "Hai i privilegi di admin!\n"

eseguiAzionePrivilegiata :: User -> Maybe String
eseguiAzionePrivilegiata = liftM azionePrivilegiata . privilege

-- Utenti legittimi
adminLucio      = User "Lucio" (Just $ PrivilegeLevel Admin)
simpleUserMatty = User "Matty" Nothing

-- Esempio di privilegio non-privilegiato.
maliciusUserPrivilege :: Privilege 'SimpleUser
maliciusUserPrivilege  = PrivilegeLevel SimpleUser

{- Esempio di cosa che solleva un errore di tipo:

azionePrivilegiata :: Privilege 'Admin -> String
maliciusUserPrivilege :: Privilege 'SimpleUser

> azionePrivilegiata maliciusUserPrivilege

-- <interactive>:22:20-40: error:
--     • Couldn't match type ‘'SimpleUser’ with ‘'Admin’
--       Expected type: Privilege 'Admin
--         Actual type: Privilege 'SimpleUser

-}

main = do
  print $ eseguiAzionePrivilegiata adminLucio
  print $ eseguiAzionePrivilegiata simpleUserMatty


data StrangeList t =
    StrangeEmpty Nat
  | StrangeCons  Nat t (StrangeList t)
