module Game (JocMonad(..), Mode(..), juga, estatInicial) where

import Control.Monad (unless)

import Utils (todo, maybeHead)
import Controller (Input(..), viToInput)
import Parser (Instance(..), parseInstance) -- Importem la instancia del parser per poder llegir el fitxer i fer proves

--IMPRTANT!!!!
-- Aquest primer commit sobre aquest fitxer en concret, es simplement una base, no es segur de que sigui aixis, mentre funcioni, pots canviar coses
-- Ara mateix el que tinc fet, entenc perfectament fins al primer mètode de instance show JocMontad
-- A partir de aquest metode mes o menys veig el que vaig fent, estic copiant jo a ma el codi pero no acabo de pillar que es el que estic fent del tot
-- Perque ho sapigues, el codi de Parser.hs si que el podria donar per fet, aquest si que es facil i s'enten be, si no entens m'ho dius

type posicio = (int, int)
type tauler = [string]
  
data Nucli = Nucli [posicio]
  deriving show ( Eq,show)

-- This of course need to be adapted to your needs
data JocMonad = JocMonad
{
  alcadaNucli :: Int,
  nColumnes :: Int,
  nFiles :: Int,
  tauler :: tauler,
  inici :: posicio,
  reactor :: posicio,
  nucli :: Nucli
} deriving (Eq)


instance Show JocMonad where
  show joc = 
    unlines [mostraFila r Fila | (r,Fila) <- zip [0..] (tauler joc)]
    where
      posNucli = localitzaNucli (nucli joc)

      mostraFila :: Int -> String -> String
      mostraFila r fila = [mostraCasella (r,c) valor | (c,valor) <- zip [0..] fila]
      
      mostraCasella :: posicio -> Char -> Char
      mostraCasella p valor
        | p 'elem' posNucli = 'N'
        | otherwise = valor


-- Mode de joc, pot ser AI, QUE HO FACI LA MAQUINA, O QUE HO FACI L'USUARI, ES A DIR NOSALTRES
data Mode = AI | Huma
-- I give you a dummy initial state, modify it as you need
estatInicial :: Instance -> JocMonad
estatInicial (Instance h cols rows t) = 
  JocMonad
  {
    alcadaNucli = h,
    nColumnes = cols,
    nFiles = rows,
    tauler = t,
    inici = posInicial,
    reactor = posFinal,
    nucli = Nucli [posInicial]
  }
  where
    posInicial = trobaCaracter 'S' t -- Metodes per trobar on esta el caracter S i G que representen el que seria la posicio inicial des de on començem,
    -- i la posicio final a la qual tenim com ha objectiu arribar al acabar l'execució
    posFinal = trobaCaracter 'G' t
  
  trobaCaracter :: Char -> Tauler -> posicio
  trobaCaracter ch t =
    case [(r,c) | (r,fila) <- zip [0..] t,
                  (c,valor) <- zip [0..] fila,
                  valor == ch] of
      (p:_) -> p
      [] -> error ("No s'ha trobat el caracter " ++ [ch] ++ " al tauler.")

-- This is where the magic happens, you need to finish this function to make the JocMonad work,
-- but I give you a dummy implementation to start with
juga :: Mode -> JocMonad -> IO()
juga AI   = todo
juga Huma = loop
    where
      loop :: JocMonad -> IO()
      loop joc = unless (finished joc) $ do
        putStrLn $ "Introdueix una direccio (h/j/k/l): "
        putStrLn $ show joc

        input <- maybeHead <$> getLine

        case input >>= viToInput of
          Nothing -> do
            putStrLn "Entrada incorrecta, torna-ho a provar."
            loop joc

          Just direc -> do
            putStrLn $ "Has entrat: " ++ show direc
            loop joc


-- PSS... this is just a dummy implementation for the example, remove it if you don't need it
finished :: JocMonad -> Bool
finished = connectatALReactor

localitzaNucli :: Nucli -> [posicio]
localitzaNucli (nucli ps) = ps

connectatALReactor :: JocMonad -> Bool
connectatAlReactor joc = 
  estaDret (nucli joc) &&
  localitzaNucli (nucli joc) == [reactor joc]

-- HE ARRIBAT FINS AQUI!!!

esPlataforma        = todo

esSegur             = todo

maniobra            = todo

succionat           = todo

soluciona           = todo