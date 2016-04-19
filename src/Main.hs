import Text.CSV
import Text.Parsec.Error
import Data.Char
import NeuralNetwork 

type Accuracy = Double
type FoldCount = Int


type AlphaFeature = String
type AlphaFeatureVector = [AlphaFeature]
type NumericFeature = Int
type NumericFeatureVector = [NumericFeature]



calculateAccuracy :: Classifier -> LabeledMushrooms -> Accuracy
calculateAccuracy classifier mushrooms = correctlyClassifiedCount / totalMushroomCount
    where correctlyClassifiedCount = fromIntegral $ getCorrectPredictionCount mushrooms classifier
          totalMushroomCount = fromIntegral $ length mushrooms


getFoldAccuracy :: FoldCount -> LabeledMushrooms -> Accuracy
getFoldAccuracy foldCount labeledMushrooms = sum accuracies / (fromIntegral foldCount) 
    where foldClassifiers = take foldCount $ repeat $ getClassifier labeledMushrooms
          partitionedData = take foldCount $ repeat labeledMushrooms -- TODO Partition data using slicing with list comprehension
          accuracies = zipWith (\classifier foldTestData -> calculateAccuracy classifier foldTestData) foldClassifiers partitionedData


alphaFeatureToNumericFeature :: AlphaFeature -> NumericFeature
alphaFeatureToNumericFeature (letter:_) = ord letter -  ord 'a'
alphaFeatureToNumericFeature [] = 0


alphaVecToNumericVec :: AlphaFeatureVector -> NumericFeatureVector
alphaVecToNumericVec = map alphaFeatureToNumericFeature 


getClassifier :: LabeledMushrooms -> Classifier
getClassifier mushrooms = \mushroom -> True


getMushroomData :: IO (Either ParseError LabeledMushrooms)
getMushroomData = convertCSV parsedCSV
    where fileName = "data.txt"
          parsedCSV = parseCSVFromFile fileName :: IO (Either ParseError [AlphaFeatureVector])


convertCSV :: IO (Either ParseError [AlphaFeatureVector]) -> IO (Either ParseError LabeledMushrooms)
convertCSV parsedCSV = inject (map getLabeledMushroom) parsedCSV
           where inject = (fmap . fmap)
           

getLabeledMushroom :: AlphaFeatureVector -> LabeledMushroom
getLabeledMushroom [] = (True, [1,2,3]) -- TODO Convert to Maybe LabeledMushroom or use Monad transformers
getLabeledMushroom (edibility : mushroom) = (isEdible edibility, mushroomFeature)
                   where isEdible = (== "e")
                         mushroomFeature = alphaVecToNumericVec mushroom 

                         
main :: IO ()
main = do
     mushrooms <- getMushroomData
     --let nodesPerLayer = 7
     --initialWeights <- getInitialWeights nodesPerLayer :: IO NetworkWeights
     --let nextWeights = (getNextGeneration defaultMutationRate defaultCrossoverRate) initialWeights
     -- Make infinite list of generation and pick the nth element for the final classifier
     print $ fmap (getFoldAccuracy 10) mushrooms

