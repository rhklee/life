import qualified System.Console.ANSI as SCA
import qualified System.IO as SIO
import qualified System.Environment as Env (getArgs)
import qualified Control.Concurrent as C
import qualified Data.List as L
import Data.Foldable as F (forM_)

data Position = Position { coordinates :: (Int, Int)
                         , occupant :: Organism } 
                          deriving (Show, Eq)

-- lexical ordering
instance Ord Position where
    compare (Position (p1x, p1y) _) (Position (p2x, p2y) _) = 
        if p1x < p2x
            then LT 
            else if p1x > p2x
                then GT
                else if p1y < p2y
                    then LT
                    else if p1y > p2y
                        then GT
                        else EQ 

data OrganismType = Empty | OrganismA deriving (Show, Eq)

data Organism = Organism { oType :: OrganismType } deriving (Eq)

instance Show Organism where
    show organism 
        | oType organism == Empty = " "
        | oType organism == OrganismA = "•"

type RegionDim = (Int, Int)

data Region = Region { regionDim :: RegionDim,
                       positions :: [Position] } deriving (Eq)


instance Show Region where
    show region = L.unlines (foldr1 (<>) <$> (map (show . occupant)) 
                                         <$> L.groupBy     
                                              (\p1 p2 -> (fst . coordinates $ p1) == (fst . coordinates $ p2)) 
                                              (L.sort (positions region)))

modN :: Eq a => Integral a => a -> a -> a
modN n m
    | n >= 0 = n `mod` m
    | otherwise = until (\x -> x >= 0) (+m) n

getNeighbors :: Region -> Position -> [Position]
getNeighbors r p = 
    let subjectCoords = coordinates p
        xC = fst subjectCoords
        yC = snd subjectCoords
        xRd = fst . regionDim $ r
        yRd = snd . regionDim $ r 
        nbrCoords = [ 
            (modN (xC + xCoord) xRd, modN (yC + yCoord) yRd) | 
            xCoord <- [-1,0,1], yCoord <- [-1,0,1], xCoord /=0 || yCoord /= 0
                                                ] in
        filter (\p -> (coordinates p) `elem` nbrCoords) (positions r)


threeAliveNeighbors :: [Position] -> Bool
threeAliveNeighbors ps = 
    let numAlive = length $ (filter (\p -> (oType . occupant $ p) /= Empty) ps) in
        numAlive == 3 

-- 2 or 3 alive neighbors
survivingNeighbors :: Position -> [Position] -> Bool
survivingNeighbors subject ps = 
    let orgType = oType . occupant $ subject 
        numSelfSame = length $ (filter (\p -> (oType . occupant $ p) == orgType) ps) in
        numSelfSame == 2 || numSelfSame == 3

castJudgement :: Region -> Position -> Judgement
castJudgement region position = 
    let nbrs = getNeighbors region position in
        case oType . occupant $ position of
            Empty -> if threeAliveNeighbors nbrs then Birth else Death
            OrganismA -> if survivingNeighbors position nbrs then Survival else Death

data Judgement = Survival | Death | Birth deriving (Show)

positionTick :: Region -> Position -> Position
positionTick region position = 
    case castJudgement region position of
        Survival -> position
        Death -> Position (coordinates position) (Organism Empty)
        Birth -> Position (coordinates position) (Organism OrganismA)

tick :: Region -> Region
tick region = Region (regionDim region) $ positionTick region <$> (positions region)


xCoords :: Int 
xCoords = 10

yCoords :: Int
yCoords = 10

initRegion :: Region
initRegion = Region (xCoords,yCoords) [ Position (x,y) org | x<-[0..(xCoords-1)], y<-[0..(yCoords-1)], 
                            let org = if (x == 3 && y `elem` [1..3]) ||
                                         (x == 2 && y == 3) ||
                                         (x == 1 && y == 2)
                                         then Organism OrganismA else Organism Empty ]

constructInitRegion :: Int -> Int -> [Position] -> Region
constructInitRegion xDim yDim ps = Region (xDim, yDim) ps

second :: Int
second = 400000

delayClear :: Int -> IO()
delayClear n = do
    C.threadDelay n
    SCA.clearScreen

toStringDelay :: IO() -> Region -> IO ()
toStringDelay delayer r = (putStrLn . show $ r) >> delayer

initFile :: String
initFile = "life_config"

oA :: Char 
oA = 'o'

oE :: Char
oE = '_'

constructPositions :: Int -> Int -> [String] -> [Position]
constructPositions nRows nCols ls = 
    let labeledInput = labelCols $ labelRows ls in
        [ Position (snd rowEntry, snd labeledCols) o | 
                             labeledCols <- labeledInput, 
                             rowEntry <- (fst labeledCols),
                             let o = if (fst rowEntry) == oA 
                                        then (Organism OrganismA)
                                        else (Organism Empty) ]

labelRows :: [String] -> [(String, Int)]
labelRows s = zip s [0..]

labelCols :: [(String, Int)] -> [ ([ (Char, Int) ], Int) ]
labelCols rLabeledStrs =
    [ (zip (fst lStr) [0..], snd lStr) | lStr <- rLabeledStrs ]

-- Run with
-- stack exec -- runghc Life.hs life_config
main :: IO()
main = do
    [fileName] <- Env.getArgs

    -- Read in file
    handle <- SIO.openFile fileName SIO.ReadMode
    contents <- SIO.hGetContents handle
    let ls = lines contents
    let nCols = length $ head $ ls
    let nRows = length ls
    let startingRegion = constructInitRegion nRows nCols $ constructPositions nRows nCols ls
    putStrLn $ show startingRegion

    getLine
    delayClear second

    F.forM_ (iterate tick startingRegion) (toStringDelay (delayClear second))






