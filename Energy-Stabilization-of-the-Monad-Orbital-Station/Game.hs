module Game (JocMonad(..), Mode(..), juga, estatInicial) where

import Control.Monad (unless)

import Utils (todo, maybeHead)
import Controller (Input(..), viToInput)


-- This of course need to be adapted to your needs
data JocMonad = JocMonad Int

instance Show JocMonad where
  show _ = "PSS... finish me!"


-- Mode de joc
data Mode = AI | Huma


--
-- I give you a dummy initial state, modify it as you need
--
estatInicial = todo


-- This is where the magic happens, you need to finish this function to make the JocMonad work,
-- but I give you a dummy implementation to start with
juga :: Mode -> JocMonad -> IO()
juga AI   = todo
juga Huma = loop
    where
      -- I give you this example of JocMonad loop,
      -- but you can change it as you need
      loop :: JocMonad -> IO()
      loop joc@(JocMonad n) = unless (finished joc) $ do
        putStrLn $ "You have " ++ show n ++ " moves left. Enter a direction (h/j/k/l): "
        putStrLn $ show joc
        input <- maybeHead <$> getLine
        case input >>= viToInput of
          Nothing -> do
            putStrLn "Invalid input, try again."
            loop joc
          Just direc -> do
            putStrLn $ "You entered: " ++ show direc
            loop $ JocMonad (n - 1)


-- PSS... this is just a dummy implementation for the example, remove it if you don't need it
finished :: JocMonad -> Bool
finished (JocMonad n)
  | n == 0    = True
  | otherwise = False


localitzaNucli      = todo

connectatAlReactor  = todo

esPlataforma        = todo

esSegur             = todo

maniobra            = todo

succionat           = todo

soluciona           = todo