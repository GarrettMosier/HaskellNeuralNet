
type Weight = Double
type LayerWeights = [Weight]
type NetworkWeights = [LayerWeights]
type GenerationOfNetworkWeights = [NetworkWeights]

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


makeClassifierWithWeights :: NetworkWeights -> Classifier
makeClassifierWithWeights (initialWeights:restWeights) = classifier
-- reduce for each layer in the network and check the last value
    where classifier = \_ -> True
          --myClass = foldl reduceFunction initialWeights restWeights
          reduceFunction singleLayerWeights singleLayerInput = map mapFunc singleLayerWeights 
          mapFunc singleLayerInput = boolToWeight . (isActive singleLayerInput)           
          boolToWeight True = 1.0
          boolToWeight False = 0.0



