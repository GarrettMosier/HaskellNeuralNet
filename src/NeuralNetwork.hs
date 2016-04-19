module NeuralNetwork where

import Control.Monad
import System.Random
import Data.List
import Data.Function


type Weight = Double
type LayerWeights = [Weight]
type NetworkWeights = [LayerWeights]
type GenerationOfNetworkWeights = [NetworkWeights]

type NodeInput = LayerWeights

type MutationRate = Double
type CrossoverRate = Double

type Classifier = Mushroom -> Edibile
type Edibile = Bool

type Mushroom = [Int]
type LabeledMushroom = (Edibile, Mushroom)
type LabeledMushrooms = [LabeledMushroom]

type NodesPerLayer = Int


defaultCrossoverRate :: CrossoverRate
defaultCrossoverRate = 0.6

defaultMutationRate :: MutationRate
defaultMutationRate = 0.05

getCorrectPredictionCount :: LabeledMushrooms -> Classifier -> Int
getCorrectPredictionCount mushrooms classifier = length $ filter correctlyClassified mushrooms
                          where correctlyClassified (edibility, mushroom) = classifier mushroom ==  edibility


-- Can likely just pass in * as an operator
-- Make sure both are same size
isActive :: NodeInput -> LayerWeights -> Bool
isActive input weights = summedInput > 0
    where summedInput = sum $ zipWith (*) input weights


getRankedPopulation :: LabeledMushrooms -> GenerationOfNetworkWeights -> GenerationOfNetworkWeights
getRankedPopulation mushrooms unrankedWeights = rankedWeights
    where rankedWeights = map fst rankedWeightsWithAccuracy
          classifiers = map makeClassifierWithWeights unrankedWeights
          accuracies = map (getCorrectPredictionCount mushrooms) classifiers
          unrankedWeightsWithAccuracy = zip unrankedWeights accuracies
          rankedWeightsWithAccuracy  = sortBy (compare `on` snd) unrankedWeightsWithAccuracy


updateWeights :: LabeledMushrooms -> NetworkWeights -> NetworkWeights 
updateWeights mushrooms weights = weights


crossover :: NetworkWeights -> NetworkWeights -> CrossoverRate -> IO NetworkWeights
crossover w1 w2 crossoverRate = fmap (pickSide w1 w2) (randomIO :: IO Bool)
    where pickSide optOne optTwo b = if b then optOne else optTwo


-- Taken from Stack Overflow
pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
 

mutateWeights :: NetworkWeights -> MutationRate -> IO NetworkWeights
mutateWeights weights mutationRate = return weights
    where mutationOperators = [(+), (*), (-), (/)]
          potentiallyMutateWeight = fmap (> mutationRate) (randomIO :: IO Double)
          selectOperator = pick mutationOperators 
          --blah = traverse
          -- Use liftM2 to get both the weight and boolean?
          pickWeight = \weight -> \x -> if x then weight else weight



traverse :: (Weight -> Weight) -> NetworkWeights -> NetworkWeights
traverse operator val = (fmap . fmap) operator val


getNextGeneration :: MutationRate -> CrossoverRate -> NetworkWeights -> NetworkWeights
getNextGeneration mutationRate crossoverRate weights = weights



getInitialWeights :: NodesPerLayer -> IO NetworkWeights
getInitialWeights nodesPerLayer = replicateM layerCount $ replicateM nodesPerLayer (randomIO :: IO Weight)
    where layerCount = 5


-- TODO Use liquid Haskell to make sure this is a non-empty list
makeClassifierWithWeights :: NetworkWeights -> Classifier
makeClassifierWithWeights (initialWeights:restWeights) = classifier
-- reduce for each layer in the network and check the last value
    where classifier = \_ -> True
          --myClass = foldl reduceFunction initialWeights restWeights
          reduceFunction singleLayerWeights singleLayerInput = map mapFunc singleLayerWeights 
          mapFunc singleLayerInput = boolToWeight . (isActive singleLayerInput)           
          boolToWeight True = 1.0
          boolToWeight False = 0.0



