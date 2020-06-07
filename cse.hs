import Data.List
import Data.Char
import System.IO 


data Ninja = Ninja {
    name:: String, 
    country:: Char,
    status:: String, 
    exam1:: Double,
    exam2:: Double, 
    ability1:: String,
    ability2:: String, 
    r::Int,
    score:: Double
} deriving Show


data NinjaCatalogue = NinjaCatalogue {
    fire :: [Ninja], 
    lightning :: [Ninja],
    water :: [Ninja], 
    wind :: [Ninja],
    earth :: [Ninja]
} deriving Show


-- HELPER FUNCS --

-- Converts from verbose country name to lower case country code
countryVerboseToCode :: String -> Char
countryVerboseToCode s
    | [toLower low | low <- s] == "earth" = 'e' 
    | [toLower low | low <- s] == "lightning" = 'l'
    | [toLower low | low <- s] == "water" = 'w'
    | [toLower low | low <- s] == "wind" = 'n'
    | [toLower low | low <- s] == "fire" = 'f'

-- Converts from country code to verbose lower case country name
countryCodeToVerbose :: Char -> String
countryCodeToVerbose c
    | (toLower c) == 'e' = "earth"
    | (toLower c) == 'l' = "lightning"
    | (toLower c) == 'w' = "water"
    | (toLower c) == 'n' = "wind"
    | (toLower c) == 'f' = "fire"

-- Converts abilities to their corresponding impacts in score calculation
abilityImpact :: String -> Int
abilityImpact ability
    | ability == "Clone" = 20
    | ability == "Hit" = 10
    | ability == "Lightning" = 50
    | ability == "Vision" = 30
    | ability == "Sand" = 50
    | ability == "Fire" = 40
    | ability == "Water" = 30
    | ability == "Blade" = 20
    | ability == "Summon" = 50
    | ability == "Storm" = 10
    | ability == "Rock" = 20


-- Calculates the initial score during Ninja creations
calcInitScore :: Double -> Double -> String -> String -> Double
calcInitScore exam1 exam2 ability1 ability2 = 
    (0.5 * exam1) + (0.3 * exam2) + (fromIntegral $ abilityImpact ability1) + (fromIntegral $ abilityImpact ability2)


-- Creates a Ninja from a line parsed from the input file
stringToNinja :: String -> Ninja
stringToNinja input = ninja 
    where 
        features = words input
        ninja :: Ninja
        ninja = Ninja {
                    name = features !! 0,
                    country = countryVerboseToCode (features !! 1),
                    status = "junior",
                    exam1 = read (features !! 2) :: Double,
                    exam2 = read (features !! 3) :: Double,
                    ability1 = features !! 4,
                    ability2 = features !! 5,
                    r = 0, 
                    score = (
                        calcInitScore 
                        (read (features !! 2) :: Double)
                        (read (features !! 3) :: Double)
                        (features !! 4)
                        (features !! 5)
                    )
                    }

-- Distributes Ninjas to their respective countries 
distributeNinjas :: Char -> [Ninja] -> [Ninja]
distributeNinjas place ninjaList 
    | null ninjaList = []
    | country (head ninjaList) == place = (head ninjaList) : distributeNinjas place (tail ninjaList)
    | otherwise = distributeNinjas place (tail ninjaList)
    

-- Creates the Ninja Catalogue
createNinjaCatalogue :: [String] -> NinjaCatalogue
createNinjaCatalogue ninjas = catalogue
    where
        ninjaList = map stringToNinja ninjas
        -- ninjaList = parseList ninjas -- used map instead
        catalogue = NinjaCatalogue {fire = distributeNinjas 'f' ninjaList , 
                                    lightning = distributeNinjas 'l' ninjaList , 
                                    water = distributeNinjas 'w' ninjaList , 
                                    wind = distributeNinjas 'n' ninjaList , 
                                    earth = distributeNinjas 'e' ninjaList
                                    }


viewNinjaInfo :: Ninja -> String
viewNinjaInfo n = 
    (name n) ++ ", Score: " ++ (show $ score n) ++ ", Status: " ++ (status n) ++ ", Round: " ++ (show $ r n)


-- Prints sorted list of all ninjas
viewAllCountries :: NinjaCatalogue -> IO ()
viewAllCountries catalogue = do 
                            let ninjaList = unlines $ map viewNinjaInfo 
                                                            (quickSort (fire catalogue ++ 
                                                            earth catalogue ++ 
                                                            lightning catalogue ++ 
                                                            water catalogue ++ 
                                                            wind catalogue))
                            putStrLn ninjaList
                            return()

-- Prints ninjas of specific country
ninjasOfCountry :: NinjaCatalogue -> String -> IO ()
ninjasOfCountry catalogue response = do
    if response /= "" && response /= "error"
        then putStrLn response
        else do 
                if response == "error"
                    then putStrLn "The country code you have entered is not valid!\n"
                else do
                    putStrLn "Enter e for Earth, w for Water, f for Fire, l for Lightning and n for Wind"
                putStrLn "Enter country code: "
                code <- getLine
                case code of 
                    "e" -> ninjasOfCountry catalogue (unlines ( map viewNinjaInfo (quickSort ( earth catalogue))))
                    "l" -> ninjasOfCountry catalogue (unlines ( map viewNinjaInfo (quickSort ( lightning catalogue))))
                    "w" -> ninjasOfCountry catalogue (unlines ( map viewNinjaInfo (quickSort ( water catalogue))))
                    "n" -> ninjasOfCountry catalogue (unlines ( map viewNinjaInfo (quickSort ( wind catalogue))))
                    "f" -> ninjasOfCountry catalogue (unlines ( map viewNinjaInfo (quickSort ( fire catalogue))))
                    _ -> ninjasOfCountry catalogue "error"
    return()


rootPrompt :: NinjaCatalogue  -> IO ()
rootPrompt catalogue  = do
    let options  = ["a) View a Country's Ninja Info 1", "b) View all Countries' Ninjas", "c) Option 3", "d) Option 3", "e) Exit" ]

    -- mapM used for mapping actions to list 
    mapM putStrLn options
    putStrLn "Type the option char and press (ENTER / RETURN)"
    s <- getLine
    putStrLn $ "You typed: " ++ s
    putStrLn ""

    case s of
        "a" -> ninjasOfCountry catalogue ""
        "b" -> viewAllCountries catalogue
        "e" -> return ()
    
    if s == "e"
        then return ()
        else do rootPrompt catalogue

 

-- Sorts the Ninjas according to the order given in the instructions (round ascending, score descending)
quickSort :: [Ninja] -> [Ninja]
quickSort [] = []
quickSort (x:xs) = 
    quickSort larger ++ [x] ++ quickSort smaller
        where
            smaller = [a | a <- xs, compareNinjas x a]
            larger = [b | b <- xs, compareNinjas b x]


--Compares two Ninjas according to their round and score number
compareNinjas :: Ninja -> Ninja -> Bool
compareNinjas first second 
    | (r first == r second) && (score first >= score second) = True
    | r first > r second = True
    | otherwise = False

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    --args <- getArgs
    --let fileName = head args
    handle <- openFile "csereport.txt" ReadMode
    contents <- hGetContents handle
    let lineContent = lines contents
    let catalogue = createNinjaCatalogue lineContent
    rootPrompt catalogue 