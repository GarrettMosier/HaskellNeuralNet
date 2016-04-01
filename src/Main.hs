import Text.CSV
import Text.Parsec.Error
import Data.Char
import System.Random 
import Control.Monad 




type Classifier = Mushroom -> Edibile
type Edibile = Bool

type Mushroom = [Int]
type LabeledMushroom = (Edibile, Mushroom)
type LabeledMushrooms = [LabeledMushroom]

type Accuracy = Double
type FoldCount = Int

type Weight = Double
type LayerWeights = [Weight]
type NetworkWeights = [LayerWeights]

type AlphaFeature = String
type AlphaFeatureVector = [AlphaFeature]
type NumericFeature = Int
type NumericFeatureVector = [NumericFeature]

type MutationRate = Double
type CrossoverRate = Double
type NodeInput = LayerWeights
type NodesPerLayer = Int

defaultCrossoverRate :: CrossoverRate
defaultCrossoverRate = 0.6

defaultMutationRate :: MutationRate
defaultMutationRate = 0.05

-- Can likely just pass in * as an operator
-- Make sure both are same size
isActive :: NodeInput -> LayerWeights -> Bool
isActive input weights = summedInput > 0
    where summedInput = sum $ zipWith (*) input weights


getCorrectPredictionCount :: LabeledMushrooms -> Classifier -> Int
getCorrectPredictionCount mushrooms classifier = length $ filter correctlyClassified mushrooms
                          where correctlyClassified (edibility, mushroom) = classifier mushroom ==  edibility


getRankedPopulation :: LabeledMushrooms -> NetworkWeights -> NetworkWeights
getRankedPopulation mushrooms weights = weights


updateWeights :: LabeledMushrooms -> NetworkWeights -> NetworkWeights 
updateWeights mushrooms weights = weights


crossover :: NetworkWeights -> NetworkWeights -> CrossoverRate -> NetworkWeights
crossover w1 w2 crossoverRate = w1


mutateWeights :: NetworkWeights -> MutationRate -> NetworkWeights
mutateWeights weights mutationRate = weights 


getNextGeneration :: MutationRate -> CrossoverRate -> NetworkWeights -> NetworkWeights
getNextGeneration mutationRate crossoverRate weights = weights


calculateAccuracy :: Classifier -> LabeledMushrooms -> Accuracy
calculateAccuracy classifier mushrooms = doubleCorrectlyClassifiedCount / totalMushroomCount
    where doubleCorrectlyClassifiedCount = fromIntegral correctlyClassifiedCount
          correctlyClassifiedCount = getCorrectPredictionCount mushrooms classifier
          totalMushroomCount = fromIntegral $ length mushrooms


getFoldAccuracy :: FoldCount -> LabeledMushrooms -> Accuracy
getFoldAccuracy foldCount labeledMushrooms = calculateAccuracy classifier testData
    where classifier = getClassifier labeledMushrooms 
          testData = labeledMushrooms


foldValidation :: FoldCount -> LabeledMushrooms -> Accuracy
foldValidation foldCount mushrooms = sum foldAccuracies / totalMushroomCount
    where partitionedData = [mushrooms] 
          foldAccuracies = map (getFoldAccuracy foldCount) partitionedData
          totalMushroomCount = fromIntegral $ length partitionedData


alphaFeatureToNumericFeature :: AlphaFeature -> NumericFeature
alphaFeatureToNumericFeature (letter:_) = ord letter -  ord 'a'
alphaFeatureToNumericFeature [] = 0


alphaVecToNumericVec :: AlphaFeatureVector -> NumericFeatureVector
alphaVecToNumericVec = map alphaFeatureToNumericFeature 


getClassifier :: LabeledMushrooms -> Classifier
getClassifier mushrooms = \mushroom -> True


getInitialWeights :: NodesPerLayer -> IO NetworkWeights
getInitialWeights nodesPerLayer = replicateM layerCount $ replicateM nodesPerLayer (randomIO :: IO Weight)
    where layerCount = 5


makeClassifierWithWeights :: NetworkWeights -> Classifier
makeClassifierWithWeights weights = getClassifier testMushrooms

testMushrooms :: LabeledMushrooms
testMushrooms = [(True, [1,2,3])]


getMushroomData :: IO (Either ParseError LabeledMushrooms)
getMushroomData = convertCSV parsedCSV
    where fileName = "data.txt"
          parsedCSV = parseCSVFromFile fileName :: IO (Either ParseError [AlphaFeatureVector])


convertCSV :: IO (Either ParseError [AlphaFeatureVector]) -> IO (Either ParseError LabeledMushrooms)
convertCSV parsedCSV = inject (map getLabeledMushroom) parsedCSV
           where inject = (fmap . fmap)
           

getLabeledMushroom :: AlphaFeatureVector -> LabeledMushroom
getLabeledMushroom [] = (True, [1,2,3])
getLabeledMushroom (edibility : mushroom) = (isEdible edibility, mushroomFeature)
                   where isEdible = (== "e")
                         mushroomFeature = alphaVecToNumericVec mushroom 
      
main :: IO ()
main = do
     mushrooms <- getMushroomData
     let nodesPerLayer = 7
     initialWeights <- getInitialWeights nodesPerLayer :: IO NetworkWeights
     let nextWeights = (getNextGeneration defaultMutationRate defaultCrossoverRate) initialWeights
     -- Make infinite list of generation and pick the nth element for the final classifier
     print $ fmap (foldValidation 10) mushrooms

