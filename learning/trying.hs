import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import qualified Data.List as L


count' word = L.length . L.filter (==word) . L.words
    
type Ids = Set Integer

createMap :: [String] -> (Map String Ids, Map (String, Integer) Int)
createMap = L.foldr addSentence (Map.empty, Map.empty) . L.zip [0..]
    where
        addSentence (index,sentence) maps = L.foldr addItem maps . L.words $ sentence
            where
                addItem word (m1,m2) = 
                    let s = Map.findWithDefault Set.empty word m1 
                    in (Map.insert word (Set.insert index s) m1, Map.insert (word, index) (count' word sentence) m2)

createMaptoIds :: [String] -> (Map String Ids)
createMaptoIds = Map.fromListWith Set.union . foldr ((++) . zipWords) [] . zip [0..] . map words
    where
        zipWords :: (Integer, [String]) -> [(String, Set Integer)]
        zipWords (id,sentence) = zip sentence (repeat (Set.singleton id))

count word = fromIntegral . length . filter (==word)

createMaptoCounts :: [String] -> Map (String, Integer) Integer
createMaptoCounts = Map.fromList . foldr ((++) . countEach) [] . zip [0..] . map words
    where
        countEach (id, sentence) = map (\x -> ((x, id), count x sentence)) sentence


setIntersecs :: [Ids] -> Ids
setIntersecs [] = Set.empty
setIntersecs xs = L.foldr1 Set.intersection xs
    
getQueries :: Map (String, Integer) Int -> Map String Ids -> [String] -> [[Integer]]
getQueries stoCounts stoIds = L.map (getMatching . L.words) 
    where 
        getMatching :: [String] -> [Integer]
        getMatching queries = L.sort . loopThrough . setIntersecs . L.map ((flip (Map.findWithDefault Set.empty)) stoIds) $ queries
            where 
                loopThrough :: Ids -> [Integer]
                loopThrough mySet
                    | Set.null mySet = [-1]
                    | otherwise = Set.foldr getTimes [] mySet
                getTimes id arr =(L.replicate  (L.minimum . L.map ((flip (Map.findWithDefault 0)) stoCounts) $ L.zip queries (L.repeat id)) id) ++ arr
        
             
        
    

textQueries sentences queries = do
    getQueries stoCounts stoIds queries
        where (stoIds, stoCounts) = createMap sentences