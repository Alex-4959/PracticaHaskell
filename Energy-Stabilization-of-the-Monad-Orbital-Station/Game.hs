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

-- | Mostra el tauler amb el nucli representat com a 'N'
instance Show JocMonad where
  show joc = unlines [mostraFila r fila | (r, fila) <- zip [0..] (tauler joc)]
    where
      posNucli = localitzaNucli (nucli joc)

      mostraFila :: Int -> String -> String
      mostraFila r fila = [mostraCasella (r, c) valor | (c, valor) <- zip [0..] fila]

      mostraCasella :: Posicio -> Char -> Char
      mostraCasella p valor
        | p `elem` posNucli = 'N'
        | otherwise         = valor

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
-- MANIOBRA (MOVIMENT DEL NUCLI)
-- ============================================================

-- | Comprova si el nucli està tombat en direcció est-oest (mateixa fila)
esHoritzontalEW :: Nucli -> Bool
esHoritzontalEW (Nucli ps)
  | length ps <= 1 = False
  | otherwise      = let r = fst (head ps) in all (\(r',_) -> r' == r) ps

-- | Comprova si el nucli està tombat en direcció nord-sud (mateixa columna)
esHoritzontalNS :: Nucli -> Bool
esHoritzontalNS (Nucli ps)
  | length ps <= 1 = False
  | otherwise      = let c = snd (head ps) in all (\(_,c') -> c' == c) ps

-- | Moviment quan el nucli està dret (1 casella) → es tomba
mouDret :: Input -> Int -> Posicio -> Nucli
mouDret Nord h (r, c) = Nucli [(r - h + i, c) | i <- [0..h-1]]
mouDret Sud  h (r, c) = Nucli [(r + 1 + i, c) | i <- [0..h-1]]
mouDret Oest h (r, c) = Nucli [(r, c - h + i) | i <- [0..h-1]]
mouDret Est  h (r, c) = Nucli [(r, c + 1 + i) | i <- [0..h-1]]

-- | Moviment quan el nucli està tombat est-oest (mateixa fila, h caselles)
--   Nord/Sud: llisca (manté orientació), Oest/Est: s'aixeca (torna a dret)
mouHoritzontalEW :: Input -> Int -> Posicio -> Nucli
mouHoritzontalEW Nord h (r, c) = Nucli [(r - 1, c + i) | i <- [0..h-1]]
mouHoritzontalEW Sud  h (r, c) = Nucli [(r + 1, c + i) | i <- [0..h-1]]
mouHoritzontalEW Oest _ (r, c) = Nucli [(r, c - 1)]
mouHoritzontalEW Est  h (r, c) = Nucli [(r, c + h)]

-- | Moviment quan el nucli està tombat nord-sud (mateixa columna, h caselles)
--   Oest/Est: llisca (manté orientació), Nord/Sud: s'aixeca (torna a dret)
mouHoritzontalNS :: Input -> Int -> Posicio -> Nucli
mouHoritzontalNS Nord _ (r, c) = Nucli [(r - 1, c)]
mouHoritzontalNS Sud  h (r, c) = Nucli [(r + h, c)]
mouHoritzontalNS Oest h (r, c) = Nucli [(r + i, c - 1) | i <- [0..h-1]]
mouHoritzontalNS Est  h (r, c) = Nucli [(r + i, c + 1) | i <- [0..h-1]]

-- | Efectua un moviment del nucli en una direcció
maniobra :: Input -> JocMonad -> JocMonad
maniobra dir joc = joc { nucli = nouNucli }
  where
    h = alcadaNucli joc
    n = nucli joc
    (r, c) = head (localitzaNucli n)
    nouNucli
      | estaDret n       = mouDret dir h (r, c)
      | esHoritzontalEW n = mouHoritzontalEW dir h (r, c)
      | otherwise         = mouHoritzontalNS dir h (r, c)

soluciona :: JocMonad -> IO ()
soluciona = todo

juga :: Mode -> JocMonad -> IO ()
juga = todo