module Parser (
    CoreHeight,
    NColumns,
    NRows,
    TableLines,
    Instance(..),
    parseInstance
) where

import Utils (todo)

--
-- You can change the `Instance` definition if you thing it is necessary,
-- but the `parseInstance` function must be able to parse the input file into an `Instance` value.
-- You can also add any helper functions you need to implement `parseInstance`.
--

type CoreHeight = Int
type NColumns = Int
type NRows = Int
type TableLines = [String]

data Instance = Instance CoreHeight NColumns NRows TableLines deriving Show

parseInstance :: String -> Instance
parseInstance = todo