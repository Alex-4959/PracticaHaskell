module Game (JocMonad(..), Mode(..), juga, estatInicial) where

import Control.Monad (unless)

import Utils (todo, maybeHead)
import Controller (Input(..), viToInput)
import Parser (Instance(..))


-- ============================================================
-- TIPUS
-- ============================================================

-- Posició al tauler (fila, columna), 0-indexed
type Posicio = (Int, Int)

-- El tauler: guardem les línies originals del fitxer com a [String]
type Tauler = [String]

-- El nucli de fusió: guardem la llista de posicions que ocupa
data Nucli = Nucli [Posicio]
  deriving (Eq, Show)

-- Mode de joc
data Mode = AI | Huma

-- Estat complet del joc
data JocMonad = JocMonad
  { alcadaNucli :: Int       -- Alçada del nucli (unitats energètiques)
  , nColumnes   :: Int       -- Nombre de columnes
  , nFiles      :: Int       -- Nombre de files
  , tauler      :: Tauler    -- El tauler (línies originals)
  , inici       :: Posicio   -- Posició inicial (S)
  , reactor     :: Posicio   -- Posició del reactor (G)
  , nucli       :: Nucli     -- El nucli de fusió
  } deriving (Eq)


-- ============================================================
-- ESTAT INICIAL
-- ============================================================

-- | Configura l'estat inicial del joc a partir d'una instància parsejada
estatInicial :: Instance -> JocMonad
estatInicial (Instance h cols rows t) =
  JocMonad
    { alcadaNucli = h
    , nColumnes   = cols
    , nFiles      = rows
    , tauler      = t
    , inici       = posInicial
    , reactor     = posFinal
    , nucli       = Nucli [posInicial]
    }
  where
    posInicial = trobaCaracter 'S' t
    posFinal   = trobaCaracter 'G' t

-- | Troba la posició (fila, columna) d'un caràcter dins les línies del tauler
trobaCaracter :: Char -> Tauler -> Posicio
trobaCaracter ch t =
  case [(r, c) | (r, fila) <- zip [0..] t,
                 (c, valor) <- zip [0..] fila,
                 valor == ch] of
    (p:_) -> p
    []    -> error ("No s'ha trobat el caràcter " ++ [ch] ++ " al tauler.")


-- ============================================================
-- FUNCIONS BÀSIQUES DEL NUCLI
-- ============================================================

-- | Retorna les posicions ocupades pel nucli al tauler
localitzaNucli :: Nucli -> [Posicio]
localitzaNucli (Nucli ps) = ps

-- | Retorna True si el nucli està dret (vertical, ocupa 1 casella)
estaDret :: Nucli -> Bool
estaDret (Nucli ps) = length ps == 1

-- | Donada una posició del tauler, retorna True si hi ha plataforma
--   (qualsevol cosa diferent de '0' i dins dels límits)
esPlataforma :: JocMonad -> Posicio -> Bool
esPlataforma joc (r, c) =
  r >= 0 && r < nFiles joc &&
  c >= 0 && c < nColumnes joc &&
  ((tauler joc !! r) !! c) /= '0'

-- | Verifica si el nucli està vertical sobre el connector del reactor
connectatAlReactor :: JocMonad -> Bool
connectatAlReactor joc =
  estaDret (nucli joc) &&
  localitzaNucli (nucli joc) == [reactor joc]

-- | Determina si l'estat actual és segur (el nucli no cau)
esSegur :: JocMonad -> Bool
esSegur joc =
  all (esPlataforma joc) (localitzaNucli (nucli joc))

-- | Determina si el nucli ha estat succionat (alguna part fora de plataforma)
succionat :: JocMonad -> Bool
succionat joc = not (esSegur joc)


-- ============================================================
-- FUNCIONS PENDENTS (TODO)
-- ============================================================

maniobra :: Input -> JocMonad -> JocMonad
maniobra = todo

soluciona :: JocMonad -> IO ()
soluciona = todo

juga :: Mode -> JocMonad -> IO ()
juga = todo