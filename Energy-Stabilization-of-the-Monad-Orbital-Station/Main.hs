import System.IO (hFlush, stdout)
import Control.Monad

import Parser (parseInstance)
import Game (estatInicial, juga, Mode(..))

main :: IO()
main = do
    --
    -- demana el nom del fitxer .txt, el llegeix i finalment ho 'virtualitza'
    --
    putStr "Instance path: "
    hFlush stdout
    ins <- parseInstance <$> (getLine >>= readFile)
    putStrLn $ show ins

    --
    -- TODO: demanar el tipus de modalitat i cridar `juga`
    --
    let joc = estatInicial ins
    juga Huma joc