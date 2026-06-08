module Parser (
    AlcadaNucli,
    NColumnes,
    NFiles,
    TaulerLinies,
    Instance(..),
    parseInstance
) where

-- Primera línia: alçada del nucli
-- Segona línia: nombre de files
-- Tercera línia: nombre de columnes
-- Resta de línies: tauler

--He redefinit els noms dels tipus de variables, perque aixi em queda mes clar que es cada cosa
type AlcadaNucli = Int
type NColumnes = Int
type NFiles = Int
type TaulerLinies = [String]

data Instance = Instance AlcadaNucli NColumnes NFiles TaulerLinies
    deriving Show -- Per poder mostrar la instancia fent les proves del codi
    --En aquest cas he fet la prova i el resultat es correcte

parseInstance :: String -> Instance
parseInstance input =
    case netejaLinies input of
        (hLine:rowsLine:colsLine:tableLines) ->
            let h    = read hLine    :: AlcadaNucli
                rows = read rowsLine :: NFiles
                cols = read colsLine :: NColumnes
            in comprovaInstance h cols rows tableLines

        _ ->
            error "Format incorrecte: el fitxer ha de tenir l'alçada del nucli, el nombre de files, el nombre de columnes i les línies del tauler."

netejaLinies :: String -> [String]
netejaLinies =
    filter (not . null) . map (filter (/= '\r')) . lines
    -- Primer de tot dividim el text en linies, per poder llegir correctament, cada argument que ens han d'entrar
    -- Despres eliminem els possibles caracters\r per si de cas apareixen
    -- i per ultim eliminem les linies que siguin buides, per evitar problemes alhora de llegir

comprovaInstance :: AlcadaNucli -> NColumnes -> NFiles -> TaulerLinies -> Instance
comprovaInstance h cols rows tableLines
--Comprovem que les dades siguin coherents
    | length tableLines /= rows = --Comprovem que el nombre real de files del tauler coincideixi amb el especificat
        error "El nombre de línies del tauler no coincideix amb el nombre de files especificat."

    | any (\fila -> length fila /= cols) tableLines = --Comprovem que totes les files tinguin el nomre correcte de columnes
        error "El nombre de columnes en alguna línia del tauler no coincideix amb el nombre de columnes especificat."
    | otherwise =
        Instance h cols rows tableLines