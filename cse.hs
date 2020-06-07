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

getNinjaNames :: [Ninja] -> [String]
getNinjaNames [] = []
getNinjaNames y@(x:xs)
    | otherwise = (name x) : getNinjaNames xs

isJourneyMan :: Ninja -> Bool
isJourneyMan ninja
    | r ninja >= 3 = True
    | otherwise = False

checkJourneyman :: [Ninja] -> Bool
checkJourneyman [] = False
checkJourneyman y@(x:xs)
    | isJourneyMan x = True
    | otherwise = checkJourneyman xs


getStatus :: Int -> String
getStatus round 
    | round == 3 = "Journeyman"
    | otherwise = "Junior"

isName :: String -> String -> Bool
isName currentName ninjaName 
    |  currentName == ninjaName = True
    | otherwise = False

filterNinja :: String -> [Ninja] -> [Ninja]
filterNinja ninjaName [] = []
filterNinja ninjaName y@(x:xs) 
    | isName (name x) ninjaName = x : filterNinja ninjaName xs
    | otherwise = filterNinja ninjaName xs

getUpdatedCountryList :: Ninja -> [Ninja] -> [Ninja]
getUpdatedCountryList ninja [] = []
getUpdatedCountryList ninja y@(x:xs) 
    | areEqualNinjas x ninja = getUpdatedCountryList ninja xs
    | otherwise = x : getUpdatedCountryList ninja xs
        where
            areEqualNinjas :: Ninja -> Ninja -> Bool
            areEqualNinjas first second 
                | (country first == country second) && (name first == name second) = True
                | otherwise = False

updateCatalogue :: NinjaCatalogue -> Ninja -> NinjaCatalogue
updateCatalogue catalogue ninja = case (country ninja) of
    'e' -> NinjaCatalogue {fire = fire catalogue , 
                            lightning = lightning catalogue , 
                            water = water catalogue , 
                            wind = wind catalogue, 
                            earth = ninja : (getUpdatedCountryList ninja (earth catalogue))
                            }

    'f' -> NinjaCatalogue {fire = ninja : (getUpdatedCountryList ninja (fire catalogue)), 
                            lightning = lightning catalogue , 
                            water = water catalogue , 
                            wind = wind catalogue, 
                            earth = earth catalogue
                            }
    'w' -> NinjaCatalogue {fire = fire catalogue , 
                            lightning = lightning catalogue , 
                            water = ninja : (getUpdatedCountryList ninja (water catalogue)), 
                            wind = wind catalogue, 
                            earth = earth catalogue
                            }
    'n' -> NinjaCatalogue {fire = fire catalogue , 
                            lightning = lightning catalogue , 
                            water = water catalogue , 
                            wind = ninja : (getUpdatedCountryList ninja (wind catalogue)), 
                            earth = earth catalogue
                            }
    'l' -> NinjaCatalogue {fire = fire catalogue , 
                            lightning = ninja : (getUpdatedCountryList ninja (lightning catalogue)) , 
                            water = water catalogue , 
                            wind = wind catalogue, 
                            earth = earth catalogue
                            }

makeRoundBetweenNinjas :: NinjaCatalogue -> Ninja -> Ninja -> (Ninja , NinjaCatalogue)
makeRoundBetweenNinjas catalogue ninja1 ninja2 
    | (score ninja1 > score ninja2) || ((score ninja1 == score ninja2) && ((abilityImpact (ability1 ninja1)) + (abilityImpact (ability2 ninja1))) > (( abilityImpact (ability1 ninja2)) + (abilityImpact (ability2 ninja2))) )
     = (winnerNinja1 , updateCatalogue catalogue winnerNinja1)
    | otherwise = (winnerNinja2, updateCatalogue catalogue winnerNinja2)
        where
            winnerNinja1 :: Ninja
            winnerNinja1 = Ninja {name =  name ninja1, 
                        country = country ninja1,
                        status = getStatus ((r ninja1) + 1), 
                        exam1 = exam1 ninja1,
                        exam2 = exam2 ninja1, 
                        ability1 = ability1 ninja1,
                        ability2 = ability2 ninja1, 
                        r = ((r ninja1) + 1),
                        score = ((score ninja1) + 10)}
            winnerNinja2 :: Ninja
            winnerNinja2 = Ninja {name =  name ninja2, 
                        country = country ninja2,
                        status = getStatus ((r ninja2) + 1), 
                        exam1 = exam1 ninja2,
                        exam2 = exam2 ninja2, 
                        ability1 = ability1 ninja2,
                        ability2 = ability2 ninja2, 
                        r = ((r ninja2) + 1),
                        score = ((score ninja2) + 10)}

getCountryNinjas :: NinjaCatalogue -> String -> [Ninja]
getCountryNinjas catalogue code = case code of
        "w" -> water catalogue 
        "n" -> wind catalogue
        "l" -> lightning catalogue
        "e" -> earth catalogue
        "f" -> fire catalogue

getEndOfRoundMessage :: Ninja -> String
getEndOfRoundMessage winner = message 
    where
        message :: String
        message = "Winner: " ++ (name winner) ++ ", Round: " ++ (show $ r winner) ++ ", Status: " ++ (status winner) ++ "\n"

rootPrompt :: NinjaCatalogue  -> IO ()
rootPrompt catalogue  = do
    let options  = ["a) View a Country's Ninja Information", "b) View all Countries' Ninja Information", 
                    "c) Make a Round Between Ninjas", "d) Make a Round Between Countries", "e) Exit" ]
    let countries = ["w", "f", "l", "n", "e"]
    let ninjaNames = getNinjaNames (fire catalogue ++
                                    wind catalogue ++
                                    water catalogue ++
                                    lightning catalogue ++
                                    earth catalogue )

    -- mapM used for mapping actions to list 
    mapM putStrLn options
    putStrLn "Type the option char and press (ENTER / RETURN)"
    s <- getLine
    putStrLn $ "You typed: " ++ s
    putStrLn ""
    
    if (s == "c")
        then do
        -- C
        putStrLn "Enter the name of the first Ninja: "
        name1 <- getLine
        -- Check if name1 is valid
        if elem name1 ninjaNames
            then do 
            putStrLn "Enter the country code of the first Ninja: "
            country1 <- getLine
            -- Check if country1 is valid
            if elem country1 countries
                then    
                if (country1 == "f" && (elem name1 (getNinjaNames (fire catalogue)))) 
                    || (country1 == "n" && elem name1 (getNinjaNames (wind catalogue))) 
                    || (country1 == "l" && elem name1 (getNinjaNames (lightning catalogue))) 
                    || (country1 == "w" && elem name1 (getNinjaNames (water catalogue))) 
                    || (country1 == "e" && elem name1 (getNinjaNames (earth catalogue))) 
                    then do
                    -- Country is valid
                    -- Check for Journeyman
                    if (not (null country1) && country1 == "w" && not(checkJourneyman (water catalogue)))
                        || (country1 == "n" && not(checkJourneyman (wind catalogue)))
                        || (country1 == "l" && not(checkJourneyman (lightning catalogue)))
                        || (country1 == "f" && not(checkJourneyman (fire catalogue)))
                        || (country1 == "e" && not(checkJourneyman (earth catalogue)))
                        then do 
                        -- Checks for Ninja 1 are complete go on to Ninja 2
                        putStrLn "Enter the name of the second Ninja: "
                        name2 <- getLine
                        -- Check if name2 is valid
                        if elem name2 ninjaNames
                            then do 
                            putStrLn "Enter the country code of the first Ninja: "
                            country2 <- getLine
                            -- Check if country2 is valid
                            if elem country2 countries
                                then    
                                if (country1 == "f" && (elem name2 (getNinjaNames (fire catalogue)))) 
                                    || (country2 == "n" && elem name2 (getNinjaNames (wind catalogue))) 
                                    || (country2 == "l" && elem name2 (getNinjaNames (lightning catalogue))) 
                                    || (country2 == "w" && elem name2 (getNinjaNames (water catalogue))) 
                                    || (country2 == "e" && elem name2 (getNinjaNames (earth catalogue))) 
                                    then do
                                    -- Country is valid
                                    -- Check for Journeyman
                                    if (not (null country1) && country2 == "w" && not(checkJourneyman (water catalogue)))
                                        || (country2 == "n" && not(checkJourneyman (wind catalogue)))
                                        || (country2 == "l" && not(checkJourneyman (lightning catalogue)))
                                        || (country2 == "f" && not(checkJourneyman (fire catalogue)))
                                        || (country2 == "e" && not(checkJourneyman (earth catalogue)))
                                        then do 
                                            if ((country1 == country2) && (name1 == name2))
                                                then do 
                                                putStrLn "The ninjas selected are the same ninjas. Pleasse try again!\n"
                                                return() 
                                                else do
                                                -- Checks for Ninja 1 and Ninja 2 are complete
                                                putStrLn " Ninjas are going in a fight... \n "
                                                let ninja1 = filter (\a -> (name a) == name1) (getCountryNinjas catalogue country1)
                                                let ninja2 = filter (\a -> (name a) == name2) (getCountryNinjas catalogue country2)
                                                let winner  = fst (makeRoundBetweenNinjas catalogue (head ninja1) (head ninja2))
                                                let newCatalogue  = snd (makeRoundBetweenNinjas catalogue (head ninja1) (head ninja2))
                                                putStrLn $ getEndOfRoundMessage winner
                                                return() 
                                        else do 
                                        putStrLn "There is already a Journeyman in the country of the second Ninja. Pleasse try again!\n"
                                        return() 
                                    else do
                                    putStrLn "Ninja 2 not in this country\n"
                                    return()
                                else do
                                putStrLn "Country entered for the second ninja does not exist. Please try again!\n"
                                return()

                            else do
                            putStrLn "Wrong name is given for the second Ninja. Please try again!\n"
                            return()
                        else do
                        putStrLn "There is already a Journeyman in the country of the first Ninja. Pleasse try again!\n"
                        return() 
                    else do
                    putStrLn "Ninja 1 not in this country\n"
                    return()
                else do 
                putStrLn "Country entered for the first ninja does not exist. Please try again!\n"
                return()
            else do
            putStrLn "Wrong name is given for the first Ninja. Please try again!\n"
            return()
        -- C
        else 
        if (s == "a")
            then ninjasOfCountry catalogue ""
            else do 
            if (s == "b")
                then viewAllCountries catalogue
                else do 
                putStrLn "" 
    
        
    if (s == "e") 
        then return ()
        else do 
        rootPrompt catalogue

 

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