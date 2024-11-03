--import qualified Data.List
import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
type PriorityQueue = Heap (Distance, Int)

-- HEAP

data Heap a = Empty
            | Node a (Heap a) (Heap a)
            deriving (Show, Eq)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Node x left1 right1) h2@(Node y left2 right2)
    | x <= y    = Node x left1 (merge right1 h2)
    | otherwise = Node y left2 (merge h1 right2)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (Node x Empty Empty) h

findMin :: Ord a => Heap a -> a
findMin (Node x _ _) = x

removeMin :: Ord a => Heap a -> Heap a
removeMin Empty = Empty
removeMin (Node _ left right) = merge left right

size :: Heap a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

populateQueue :: PriorityQueue -> Int -> Int -> PriorityQueue
populateQueue q size source 
    | size == 0 = q
    | size == source = populateQueue q (size -1) source
    | otherwise = populateQueue (insert (maxBound, size) q) (size -1) source
    

-- SET

--Operated in binary. A number n (n>0) is in the set if the bit (n+1) (from right to left) is 1 and isn't in the set if the bit is 0
type Set = Int

--Returns the empty set
emptySet :: Set
emptySet = 0

--Checks if the set is empty
isEmptySet :: Set -> Bool
isEmptySet s = s == 0

--Returns a set with all numbers from 1 to the given argument
fullSet :: Int -> Set
fullSet x = 2^(x+1) - 2

--Deletes a number from the set
delFromSet :: Set -> Int -> Set
delFromSet s i = d'*e+m
    where (d,m) = divMod s e
          e = 2^i
          d' = if odd d then d-1 else d

--Returns the next set from all possible sets
nextSet :: Set -> Set
nextSet s = s + 2

--Returns a list with all numbers that are in a set
setToList :: Set -> [Int]
setToList s = s2l s 0
    where s2l 0 _ = []
          s2l n i | odd n = i : s2l (n `div` 2) (i+1)
                  | otherwise = s2l (n `div` 2) (i+1)

-- TABLE

newtype Table a b = Tbl (b -> a)

--Creates a new table from a list of tuples
newTable :: Eq b => [(b,a)] -> Table a b
newTable = foldr updTable (Tbl (\_ -> error "updTable:item not found"))

--Finds the value for a given index
findTable :: Table a b -> b -> a
findTable (Tbl f) i = f i

--Updates a table entry
updTable :: Eq b => (b, a) -> Table a b -> Table a b
updTable (i, x) (Tbl f) = Tbl g
    where g j | j == i = x
              | otherwise = f j  

-- TSP TABLE

type TspTable = Table TspEntry TspCoord

type TspEntry = (Int, [Int])
type TspCoord = (Int, Set)

-- SP table

type SpTable = Table Distance Int
type PrevTable = Table [Int] Int

--Converts a RoadMap in a Ajacency Matrix
toAdjMatrix :: RoadMap -> AdjMatrix
toAdjMatrix g = Data.Array.array bounds [((i,j), distance g c1 c2) | (i, c1) <- zip [1..] clist, (j, c2) <- zip [1..] clist]
    where n = length clist
          clist = cities g
          bounds = ((1,1), (n,n))

--Converts a city to an int
cityToInt :: RoadMap -> City -> Int
cityToInt g c = head [i | (i,city) <- zip [1..] (cities g), city == c]

--Converts an int to a city
intToCity :: RoadMap -> Int -> City
intToCity g c = head [city | (i,city) <- zip [1..] (cities g), i == c]

--Returns the distance between two cities if a road exists and returns infinite otherwise
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
areAdjacent [] _ _ = False
areAdjacent ((a,b,c):xs) c1 c2
    | (a == c1 && b == c2) || (a == c2 && b == c1 ) = True
    |  otherwise = areAdjacent xs c1 c2

-- Returns Just distance between two cities if connected directly. Else returns Nothing.
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance (x:xs) a b
    | (a,b) == cities || (b,a) == cities = Just (getDistance x)
    | otherwise = distance xs a b
    where cities = getCities x

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] c = []
adjacent ((c1,c2,d):xs) c 
    | c1 == c = (c2,d):adjacent xs c
    | c2 == c = (c1,d):adjacent xs c
    | otherwise = adjacent xs c

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (c1:c2:cs) =
    case distance roadmap c1 c2  of
        Just dist -> case pathDistance roadmap (c2:cs) of 
            Just rest -> Just (dist + rest)
            Nothing -> Nothing

        Nothing -> Nothing


--Returns the names of the cities with the highest number of roads connecting to them
rome :: RoadMap -> [City]
rome g = romeAux (getDegrees g) [] 0

--Auxiliar function for rome 
--[(City, Integer)] - List with all cities and respective degree
--[City] - Cities with higher degree
--Integer - Greatest degree so far
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


--Returns True if roadmap is strongly connected, since it is undirected, runs a dfs and compares it with the full list of cities
--Roadmap - Graph
-- Bool - True if strongly connected 
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected r =  length (dfs (head (cities r)) []) == length (cities r)
                            where
                                dfs :: City -> [City] -> [City]
                                dfs c visited | c `elem` visited = visited
                                              | otherwise =  foldr dfs (c : visited) adj
                                                where adj = map fst (adjacent r c)

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap c1 c2 
    | c1 == c2 = [[c1]]
    | otherwise = map (map (intToCity roadmap)) (dijkstra adjMatrix pq distSource prevCity source target n)
        where
            adjMatrix = toAdjMatrix roadmap
            pq = populateQueue (insert (0,source) Empty) n source
            distSource = newTable [if city == source then (city,0) else (city,maxBound) | city <- [1..n]]
            prevCity = newTable [(city,[]) | city <- [1..n]]
            source = cityToInt roadmap c1
            target = cityToInt roadmap c2
            n = length (cities roadmap)
            

dijkstra :: AdjMatrix -> PriorityQueue -> SpTable -> PrevTable -> Int -> Int -> Int -> [[Int]]
dijkstra adjMatrix Empty _ prevCity source target n = recursivePath prevCity source target [] []
dijkstra adjMatrix pq distSource prevCity source target n = dijkstra adjMatrix newPQ newDistSource newPrevCity source target n
    where 
        (newDistSource, newPrevCity,newPQ) = updateDistSource adjMatrix distSource prevCity (removeMin pq) Empty u 
        u = snd(findMin pq)

updateDistSource:: AdjMatrix -> SpTable -> PrevTable -> PriorityQueue -> PriorityQueue -> Int ->  (SpTable, PrevTable, PriorityQueue)
updateDistSource adjMatrix dist prev Empty pq u = (dist,prev,pq)
updateDistSource adjMatrix dist prev pq newPQ u  
    | dist2 == maxBound = updateDistSource adjMatrix dist prev (removeMin pq) (insert (distv, v) newPQ) u 
    | alt == distv = updateDistSource adjMatrix newDist newPrev (removeMin pq) (insert (alt, v) newPQ) u 
    | alt < distv = updateDistSource adjMatrix newDist newPrev2 (removeMin pq) (insert (alt, v) newPQ) u 
    | otherwise = updateDistSource adjMatrix dist prev (removeMin pq) (insert (distv, v) newPQ) u 
    where
        alt = dist1 + dist2
        dist1 = findTable dist u
        dist2 = getDist adjMatrix u v
        distv = findTable dist v
        newDist = updTable (v,alt) dist
        newPrev = updTable (v,(u:(findTable prev v))) prev
        newPrev2 = updTable (v,[u]) prev
        v = snd (findMin pq) 



recursivePath :: PrevTable -> Int -> Int -> [Int] -> [[Int]] -> [[Int]]
recursivePath prevCity source target path acc 
    | previous == [] = if path == [] then acc else (source:path):acc
    | otherwise = buildPath prevCity source previous (target:path) acc
    where
        previous = findTable prevCity target

buildPath :: PrevTable -> Int -> [Int] -> [Int] -> [[Int]] -> [[Int]]
buildPath prevCity source [] path acc = acc
buildPath prevCity source (p:ps) path acc = buildPath prevCity source ps path (recursivePath prevCity source p path acc)


--Returns the shortest tour using a dynamic programming approach
travelSales :: RoadMap -> Path
travelSales g = if cost == maxBound then [] else map (intToCity g) p --transform int path to city path
    where n = length (cities g)
          t = fillTableTsp (toAdjMatrix g) n (newTable []) (1, emptySet)
          (cost, p) = findTable t (n, fullSet (n-1))

--Returns an auxiliar table for TSP
--AdjMatrix - Adjacency matrix
--Int - number of cities
--TspTable - table to be filled
--TspCoord - Coordinates to the table to be filled with a value
fillTableTsp :: AdjMatrix -> Int -> TspTable -> TspCoord -> TspTable
fillTableTsp dist n t (i, s)
    | (i == (n+1)) && (s == fullSet n) = t --table filled
    | i == (n+1) = fillTableTsp dist n t (1, nextSet s) --column filled
    | otherwise = fillTableTsp dist n updatedT (i+1, s) --fill next entry in the column
    where updatedT = updTable ((i, s), fillTableEntryTsp dist n t (i, s)) t --update current entry

--Returns the value to fill in the giving coordinates
--AdjMatrix - Adjacency matrix
--Int - number of cities
--TspTable - table to be filled
--TspCoord - Coordinates to the table to be filled with a value
fillTableEntryTsp :: AdjMatrix -> Int -> TspTable -> TspCoord -> TspEntry
fillTableEntryTsp dist n t (i, s)
    | isEmptySet s = (getMaybeValue (dist Data.Array.! (i,n)), [i,n])
    | otherwise = minimum [addStart (findTable t (j, delFromSet s j)) (getMaybeValue (dist Data.Array.! (i, j))) | j <- setToList s]
    where addStart (c, p) w = if (c == maxBound) || (w == maxBound) then (maxBound, []) else (c+w, i:p)
          getMaybeValue Nothing = maxBound
          getMaybeValue (Just x) = x


--Returns the shortest tour using a brute force approach
tspBruteForce :: RoadMap -> Path
tspBruteForce g = if cost == maxBound then [] else map (intToCity g) p --transform int path to city path
    where n = length (cities g)
          (cost, p) = findTour (toAdjMatrix g) n (n, fullSet (n-1))


--Finds the shortest path starting in a city going through the cities in the set given
--AdjMatrix - Adjacency matrix
--Int - number of cities
--TspCoord - tuple with the starting city and the set
findTour :: AdjMatrix -> Int -> TspCoord -> TspEntry
findTour dist n  (i, s)
    | isEmptySet s = (getMaybeValue (dist Data.Array.! (i,n)), [i,n])
    | otherwise = minimum [addStart (findTour dist n (j, delFromSet s j)) (getMaybeValue (dist Data.Array.! (i, j))) | j <- setToList s]
    where addStart (c, p) w = if (c == maxBound) || (w == maxBound) then (maxBound, []) else (c+w, i:p)
          getMaybeValue Nothing = maxBound
          getMaybeValue (Just x) = x


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

