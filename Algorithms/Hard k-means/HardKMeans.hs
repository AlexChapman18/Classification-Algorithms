import System.IO
import Control.Monad
import Data.List
import Debug.Trace
import Data.Char

-- Types
type K = Int
type Label = Char
type Coordinates = [Float]
type Point = (Coordinates, Label)


-- Reads in the file
-- FilePath, K, initial K(s)
intakeFile :: FilePath -> [Coordinates] -> IO [Point]
intakeFile filePath clusterCentres = mainMethodOutput
  where
    mainMethodOutput = fmap (mainMethod parsedClusters)  parsedFile
    parsedClusters = parseClusters 'A' clusterCentres
    parsedFile = fmap (parseFile) rawLines
    rawLines = fmap (lines) (readFile filePath)


-- Turns Coordinates into Points
parseClusters :: Label -> [Coordinates] -> [Point]
parseClusters label [] = []
parseClusters label (headClusterPos:tailClusterPos) = (headClusterPos, label) : parseClusters newLabel tailClusterPos
  where
    newLabel = succ label


--parses the lines from the input file
parseFile :: [String] -> [Point]
parseFile rawLines = map (parsePoint 'A') rawLines


-- Converts a line into its respective point
parsePoint :: Label -> String -> Point
parsePoint label rawLine = point
  where
    point = (position, label)
    position = map (getInt) splitLine
    getInt value = read (value) :: Float
    splitLine = words (map parseChar rawLine)
    parseChar = (\x -> if or [isDigit x, isMinus x] then x else ' ')
      where
        isMinus x' = x' == '-'






-- Main method
mainMethod :: [Point] -> [Point] -> [Point]
mainMethod clusterPositions dataSet | dataSet == newDataSet = newClusters
                                    | otherwise = mainMethod newClusters newDataSet
  where
    newClusters = map (getNewClusterPos newDataSet) clusterPositions
    newDataSet = map (updatePointLabel clusterPositions) dataSet




getNewClusterPos :: [Point] -> Point -> Point
getNewClusterPos (headDataSet:tailDataSet) (cluPos, label) = (averagePosition, label)
  where
    averagePosition | (not.null) validPoints = map (/numDimensions) sumDimensions
                    | otherwise = cluPos
    numDimensions = fromIntegral( length validPoints )
    sumDimensions = foldl (zipWith (+) ) headValidPos tailValidPos
    headValidPos = (fst.head) validPoints
    tailValidPos = map fst (tail validPoints)
    validPoints = filter (\x -> (snd x) == label) (headDataSet:tailDataSet)



-- Classifies a single point
updatePointLabel :: [Point] -> Point -> Point
updatePointLabel clusterPositions dataPoint = (dataPointPos, closetLabel)
  where
    dataPointPos = fst dataPoint
    closetLabel = snd closestCluster
    closestCluster = fst $ minimumBy (\x y -> compare (snd x) (snd y)) zippedDistances
    zippedDistances = zip clusterPositions allDistances
    allDistances = map (measureDistance dataPoint) clusterPositions



-- Measures euclidean distance
measureDistance :: Point -> Point -> Float
measureDistance (coord1,_) (coord2,_) = euclidDist
  where
    euclidDist = sqrt sumSquare
    sumSquare = sum squareAbs
    squareAbs = map (**2) absDelta
    absDelta = map (abs) delta
    delta = zipWith (-) coord1 coord2

