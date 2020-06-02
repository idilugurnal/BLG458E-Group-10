import Data.List
import System.IO 

data Ninja = Ninja {
    name:: String, country:: [Char],
    status:: String, exam1:: Float,
    exam2:: Float, ability1:: String,
    ability2:: String, r::Int
} deriving Show

data NinjaCatalogue = NinjaCatalogue {
    fire :: [Ninja], lightning :: [Ninja],
    water :: [Ninja], wind :: [Ninja],
    earth :: [Ninja]
} deriving Show

stringToNinja :: String -> Ninja
stringToNinja input = ninja 
    where 
        features = words input
        ninja :: Ninja
        ninja = Ninja {name = features !! 0, country = features !! 1, status = "junior", exam1 = read (features !! 2) :: Float,
                        exam2 = read (features !! 3) :: Float, ability1 = features !! 4, ability2 = features !! 5, 
                        r = 0}

parseList :: [String] -> [Ninja]
parseList input  = case input of 
    [] -> []
    x:xs  ->  stringToNinja x : parseList xs


distributeNinjas :: String -> [Ninja] -> [Ninja]
distributeNinjas place ninjaList 
    | null ninjaList = []
    | country (head ninjaList) == place = (head ninjaList) : distributeNinjas place (tail ninjaList)
    | otherwise = distributeNinjas place (tail ninjaList)
    



createNinjaCatalogue :: [String] -> NinjaCatalogue
createNinjaCatalogue ninjas = catalogue
    where
        ninjaList = parseList ninjas
        catalogue = NinjaCatalogue {fire = distributeNinjas "Fire" ninjaList , 
                                    lightning = distributeNinjas "Lightning" ninjaList , 
                                    water = distributeNinjas "Water" ninjaList , 
                                    wind = distributeNinjas "Wind" ninjaList , 
                                    earth = distributeNinjas "Earth" ninjaList
                                    }

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile


main :: IO ()
main = do
    --args <- getArgs
    --let fileName = head args
    handle <- openFile "csereport.txt" ReadMode
    contents <- hGetContents handle
    let lineContent = lines contents
    let catalogue = createNinjaCatalogue lineContent
    print(water catalogue)