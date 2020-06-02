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
        ninja = Ninja {name = features !! 0, country = features !! 1, status = features !! 2, exam1 = read (features !! 3) :: Float,
                        exam2 = read (features !! 4) :: Float, ability1 = features !! 5, ability2 = features !! 6, 
                        r = read (features !! 7) :: Int}

parseList :: [String] -> [Ninja]
parseList input  = case input of 
    [] -> []
    x:xs  ->  stringToNinja x : parseList xs


distributeNinjas :: String -> [Ninja] -> [Ninja]
distributeNinjas place ninjaList 
    | ninjaList == [] = []
    | country (head ninjaList) == place = (head ninjaList) : distributeNinjas place (tail ninjaList)
    | otherwise = distributeNinjas place (tail ninjaList)
    

createNinjaCatalogue :: FilePath -> NinjaCatalogue
createNinjaCatalogue file = catalogue
    where
        ninjaList = parseList ninjaLines
        catalogue = NinjaCatalogue {fire = distributeNinjas "Fire" ninjaList , 
                                    lightning = distributeNinjas "Lightning" ninjaList , 
                                    water = distributeNinjas "Water" ninjaList , 
                                    wind = distributeNinjas "Wind" ninjaList , 
                                    earth = distributeNinjas "Earth" ninjaList
                                    } 


readCustomFile :: IO [String]
readCustomFile = do

    ninjas <- readFile "csereport.txt"
    return (read ninjas :: [String])



--readLines :: FilePath -> IO [String]
--readLines = fmap lines . readFile


main :: IO ()
main = do
    s <- readFile file
    print(lines s)
    createNinjaCatalogue 