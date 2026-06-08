import System.IO
import Control.Monad

import Parser (parseInstance)
import Game (estatInicial, juga, Mode(..))

main :: IO()
main = do
    --
    -- demana el nom del fitxer .txt, el llegeix i finalment ho 'virtualitza'
    --
    putStr "Instance path: "
    ins <- parseInstance <$> (getLine >>= readFile)
    putStrLn $ show ins

    --
    -- observa que has de demanar el tipus de modalitat amb la que es jugara...
    -- si tens dubtes, mira com ho he fet per poder demanar les direccions al metode `juga`
    --

    juga Huma estatInicial