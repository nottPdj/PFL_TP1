--import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)

-- SET

type Set = Int

emptySet :: Set
emptySet = 0

isEmptySet :: Set -> Bool
isEmptySet s = s == 0

fullSet :: Int -> Set
fullSet x = 2^(x+1) - 2

delFromSet :: Set -> Int -> Set
delFromSet s i = d'*e+m
    where (d,m) = divMod s e
          e = 2^i
          d' = if odd d then d-1 else d

nextSet :: Set -> Set
nextSet s = s + 2

setToList :: Set -> [Int]
setToList s = s2l s 0
    where s2l 0 _ = []
          s2l n i | odd n = i : s2l (n `div` 2) (i+1)
                  | otherwise = s2l (n `div` 2) (i+1)

-- TABLE

newtype Table a b = Tbl (b -> a)

newTable :: Eq b => [(b,a)] -> Table a b
newTable = foldr updTable (Tbl (\_ -> error "updTable:item not found"))

findTable :: Table a b -> b -> a
findTable (Tbl f) i = f i

updTable :: Eq b => (b, a) -> Table a b -> Table a b
updTable (i, x) (Tbl f) = Tbl g
    where g j | j == i = x
              | otherwise = f j  

-- TSP TABLE

type TspTable = Table TspEntry TspCoord

type TspEntry = (Int, [Int])
type TspCoord = (Int, Set)



toAdjMatrix :: RoadMap -> AdjMatrix
toAdjMatrix g = Data.Array.array bounds [((i,j), distance g c1 c2) | (i, c1) <- zip [1..] clist, (j, c2) <- zip [1..] clist]
    where n = length clist
          clist = cities g
          bounds = ((1,1), (n,n))

cityToInt :: RoadMap -> City -> Int
cityToInt g c = head [i | (i,city) <- zip [1..] (cities g), city == c]

intToCity :: RoadMap -> Int -> City
intToCity g c = head [city | (i,city) <- zip [1..] (cities g), i == c]

getDist :: AdjMatrix -> Int -> Int -> Int
getDist dist i j = getMaybeValue (dist Data.Array.! (i,j))
    where getMaybeValue Nothing = maxBound
          getMaybeValue (Just x) = x


-- Returns the cities from a edge
getCities :: (City,City,Distance) -> (City,City)
getCities (a,b,_) = (a,b)

-- Returns the distance of an edge
getDistance :: (City,City,Distance) -> Distance
getDistance (_,_,d) = d

cities :: RoadMap -> [City]
cities [] = []
cities ((c1, c2, d):xs) | elem c1 rest && elem c2 rest = rest
                        | elem c1 rest = c2 : rest
                        | elem c2 rest = c1 : rest
                        | otherwise        = c1 : c2 : rest
                        where
                            rest = cities xs

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined

-- Returns Just distance between two cities if connected directly. Else returns Nothing.
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance (x:xs) a b
    | (a,b) == cities || (b,a) == cities = Just (getDistance x)
    | otherwise = distance xs a b
    where cities = getCities x

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome g = romeAux (getDegrees g) [] 0


romeAux :: [(City, Integer)] -> [City] -> Integer -> [City]
romeAux [] l _ = l
romeAux (x:xs) l maxi
    | snd x > maxi = romeAux xs [fst x] (snd x)
    | snd x < maxi = romeAux xs l maxi
    | snd x == maxi = romeAux xs (fst x:l) maxi

--Receives a RoadMap and returns a list with all the cities and respective degree
getDegrees :: RoadMap -> [(City,Integer)]
getDegrees = foldl
      (\ degrees x -> incDegree (getCities x) (False, False) degrees)
      []

--Returns updated city's degree list after adding one edge
--(City,City) - Edge to be added
--(Bool,Bool) - False if respective degree wasn't yet updated for that city. True otherwise
--[(City,Integer)] - List with all cities and respective degree
incDegree :: (City, City) -> (Bool,Bool) -> [(City,Integer)] -> [(City,Integer)]
incDegree _ (True, True) degrees = degrees
incDegree (c1, c2) (False, True) [] = [(c1,1)]
incDegree (c1, c2) (True, False) [] = [(c2,1)]
incDegree (c1, c2) (False, False) [] = [(c1,1), (c2,1)]
incDegree (c1, c2) (b1, b2) (x:xs)
    | fst x == c1 && not b1 = (fst x, snd x + 1) : incDegree (c1,c2) (True, b2) xs
    | fst x == c2 && not b2 = (fst x, snd x + 1) : incDegree (c1,c2) (b1, True) xs
    | otherwise = x : incDegree (c1,c2) (b1, b2) xs

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined


travelSales :: RoadMap -> Path
travelSales g = if cost == maxBound then [] else map (intToCity g) p --transform int path to city path
    where n = length (cities g)
          t = fillTableTsp (toAdjMatrix g) n (newTable []) (1, emptySet)
          (cost, p) = findTable t (n, fullSet (n-1))

fillTableTsp :: AdjMatrix -> Int -> TspTable -> TspCoord -> Table TspEntry TspCoord
fillTableTsp dist n t (i, s)
    | (i == (n+1)) && (s == fullSet n) = t --table filled
    | i == (n+1) = fillTableTsp dist n t (1, nextSet s) --column filled
    | otherwise = fillTableTsp dist n updatedT (i+1, s) --fill next entry in the column
    where updatedT = updTable ((i, s), fillTableEntryTsp dist n t (i, s)) t --update current entry

fillTableEntryTsp :: AdjMatrix -> Int -> TspTable -> TspCoord -> TspEntry
fillTableEntryTsp dist n t (i, s)
    | isEmptySet s = (getMaybeValue (dist Data.Array.! (i,n)), [i,n])
    | otherwise = minimum [addStart (findTable t (j, delFromSet s j)) (getMaybeValue (dist Data.Array.! (i, j))) | j <- setToList s]
    where addStart (c, p) w = if (c == maxBound) || (w == maxBound) then (maxBound, []) else (c+w, i:p)
          getMaybeValue Nothing = maxBound
          getMaybeValue (Just x) = x


tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTestLarge :: RoadMap
gTestLarge = [ ("A", "B", 10), ("A", "C", 15), ("A", "D", 20), ("A", "E", 25),
               ("B", "C", 35), ("B", "D", 25), ("B", "E", 30),
               ("C", "D", 30), ("C", "E", 20),
               ("D", "E", 15), ("B", "F", 40), ("C", "F", 50), 
               ("D", "F", 45), ("E", "F", 35),
               ("F", "G", 60), ("A", "G", 55), ("B", "G", 65), 
               ("C", "G", 70), ("D", "G", 80), ("E", "G", 75)]

