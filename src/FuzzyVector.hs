module FuzzyVector where
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N
import           Text.Printf
import           Data.List.Split
import qualified Data.Trees.KdTree as K
import Data.List
import Distributions
import GHC.Float
import Data.Maybe
import Data.Fixed

type FuzzyVector = ( V.Vector Float -> Probability)
type Angle = Float

universe = S (-10) (-10) 10 10 1

-- | calculates the membership value of the summed two fuzzy vectors for one input vector v
-- implementation of defintion 6 in the paper
-- the input discrete space matches the output discrete space 
vecAdd :: FuzzyVector  -- ^ first fuzzy vector function
      -> FuzzyVector   -- ^ second fuzzy vector function
      -> DiscreteSpace -- ^ space dimensions to look at
      -> FuzzyVector   -- ^ resulting fuzzy vector
vecAdd myA myB space v= maximum .map (\(vi,vj) -> min (myA vi) (myB vj)) $ matchingVectors
 where matchingVectors = filter (\(vi,vj) -> vi+vj==v). createDescriteVectorsForCombination $ space


-- | calculates the membership value of the summed two fuzzy vectors for one input vector v
-- implementation of definition 7 in the paper
-- the input discrete space matches the output discrete space 
vecSub :: FuzzyVector   -- ^ first fuzzy vector function
       -> FuzzyVector   -- ^ second fuzzy vector function
       -> DiscreteSpace -- ^ space dimensions to look at
       -> FuzzyVector   -- ^ resulting fuzzy vector
vecSub myA myB space v =maximum .map (\(vi,vj) -> min (myA vi) (myB vj)) $ matchingVectors
 where matchingVectors = filter (\(vi,vj) -> vi-vj==v) . createDescriteVectorsForCombination $ space

-- | calculates the membership value of the summed two fuzzy vectors for one input vector v
--
-- TODO: Check if the input space and the output space are equal or a roundfunction has to be defined
vecMul :: FuzzyVector   -- ^ first fuzzy vector function
       -> FuzzyVector   -- ^ second fuzzy vector function
       -> DiscreteSpace -- ^ space dimensions to look at
       -> FuzzyVector   -- ^ resulting fuzzy vector
vecMul myA myB space v =maximum .map (\(vi,vj) -> min (myA vi) (myB vj)) $ matchingVectors
 where matchingVectors = filter (\(vi,vj) -> vi*vj==v) . createDescriteVectorsForCombination $ space


-- |
-- impementation of definition 11 in the paper
center :: DiscreteSpace -> FuzzyVector -> FuzzyVector
center space myA = vecSub myA myA space

type FuzzyScaleFactor = (Float -> Probability)

-- | implementation of definition 12
myLambda1 ::FuzzyVector -> FuzzyVector -> DiscreteSpace -> FuzzyScaleFactor
myLambda1 myA myB space scalefactor = maximum' . map (\v1 -> min (myA v1) (myB (crispScale scalefactor v1))) . filter (/= V.fromList [0.0,0.0]) $ createDescriteVectorsForSpace space

-- | finding the fuzzy scaling factor
--  iterieren über alle lambdas, und v
myFuzzyScale1 :: FuzzyVector -> FuzzyScaleFactor -> V.Vector Float -> [Float]
myFuzzyScale1 my fuzzyScaling v= map (\lambda-> min (my (roundVector 1 (crispScale (1/lambda) v))) (fuzzyScaling lambda)) .filter (/= 0.0)$ createDescriteLambdaSpace -- with 0.0 enlargement to minimum bound of second function, without 0.0 for lambda enlargement to maximum bound of second function

myFuzzyScale :: [Float]-> Float
myFuzzyScale = maximum'

-- | error tolerant function of Prelude.maximum
maximum' ::  [Float]  -> Float
maximum' [] = -5.0  -- if no values are found in domain changed compared to Prelude.maximum
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 
-- | 
-- make it more dependent on space, similar to fuzzyAngleDomain
createDescriteLambdaSpace :: [Float]
createDescriteLambdaSpace = [0.0,0.1..10]

crispScale :: Float ->V.Vector Float-> V.Vector Float
crispScale factor v =V.map (*factor) v 

debugFuzzyScale :: FuzzyVector -> FuzzyScaleFactor -> V.Vector Float -> [(Float, V.Vector Float,Float) ]
debugFuzzyScale my fuzzyScaling v= map (\lambda-> (lambda,v,min (my (roundVector 1 (crispScale (1/lambda) v))) (fuzzyScaling lambda))) .filter (/= 0.0)$ createDescriteLambdaSpace -- with 0.0 enlargement to minimum bound of second f

scalingTest = debugFuzzyScale mySpoon fuzzyScaling (V.fromList [3.0,3.0])
  where fuzzyScaling = myLambda1 myFork subSpoon universe 

generateScaledDistribution = map (myFuzzyScale . (myFuzzyScale1 mySpoon fuzzyScaling) ) $ createDescriteVectorsForSpace universe
 where fuzzyScaling = myLambda1 myFork subSpoon universe 


showScaledDistribution = showDistribution generateScaledDistribution universe

showDistribution :: Distribution -> DiscreteSpace -> IO ()
showDistribution dat space = mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks
  where len =round( 1+(maxX space-minX space)*(1/stepsize space))
        chuncks = chunksOf len dat

-- * fuzzy rotation
-- testFuzzyUnitVector = map (\v -> (v,fuzzyUnitVector normSpoon universe v)) (map normVector $ createDescriteVectorsForSpace universe)

testnormVector = normVector (V.fromList [1.0,1.0])

-- | normalize a FuzzyVector
-- needed for the rotation
-- implementation of definition 9
-- e has to be because we want to have the max for this e vector
-- TODO next time: result must be an angle, menge von intervallen und für jeden intervall gibt es einen my wert, generierst einen unitvector berechnest den winkel zu norden -> suchst den passenden winkelinterval und schreibst den my wert dieses passenden vektors als ergebnis

{- fuzzyUnitVector :: FuzzyVector 
                   -> DiscreteSpace
                   -> Angle
                   -> Probability
fuzzyUnitVector  myA space e = maximum' . map (myA). filter (==e) $ createDescriteUnitVectorForSpace  space 
-}

-- TODO: fuzzyrotation hier wird über den entsprechenden winkel rotiert zu norden
 

fuzzyAngleDomain ::DiscreteSpace -> [Angle]
fuzzyAngleDomain space =nub. map (myround 0 .calcAngleFromYAxisClockwise) $ createDescriteVectorsForSpace space

{-
diffAngleVector :: V.Vector Float -> V.Vector Float -> Angle
diffAngleVector v1 v2 
 | tonorthV1 <= tonorthV2 = tonorthV2-tonorthV1
 | otherwise = 360-tonorthV1+tonorthV2
 where tonorthV1 = calcAngleFromYAxisClockwise v1
       tonorthV2 = calcAngleFromYAxisClockwise v2

tetsdiff = zipWith diffAngleVector (repeat (V.fromList [1.0,0.0]))  [(V.fromList [1.0,1.0]),(V.fromList [-1.0,1.0]),(V.fromList [1.0,-1.0]),(V.fromList [-1.0,-1.0])]
-}

-- | YAxis == North direction       
calcAngleFromYAxisClockwise :: V.Vector Float -> Angle
calcAngleFromYAxisClockwise v
 | x<0=360+ (todegree$ atan2 x y) 
 | otherwise = todegree $atan2 x y
 where x = V.head v
       y = V.head.V.drop 1 $ v
       todegree b = b*180/pi 
       
-- | definition 10
myFuzzyAngle :: FuzzyVector -> FuzzyVector -> DiscreteSpace -> FuzzyAngle
myFuzzyAngle myA myB space angle = maximum' . map  (\allAngle -> min (fmyA (allAngle))  (fmyB (myround 0 (mod' (allAngle+angle)  360)  ))  ) $ fuzzyAngleDomain space
 where fmyA = fuzzyAngleOfVector myA space
       fmyB = fuzzyAngleOfVector myB space
       
myFuzzyAngle1::FuzzyVector -> FuzzyVector -> DiscreteSpace -> FuzzyAngle
myFuzzyAngle1 myA myB space angle = maximum' . map  (\allAngle -> min (fromMaybe (-5.0)  (lookup allAngle fmyA) )  (fromMaybe (-5.0)  (lookup (myround 0 (mod' (allAngle+angle)  360)  ) fmyB) )  ) $ fuzzyAngleDomain space
   where fmyA = createAngleList myA universe
         fmyB = createAngleList myB universe
         
testAngle1 = map (\a-> (a,myFuzzyAngle1 myYAxis myXAxis universe a))  $ fuzzyAngleDomain universe

-- | this angle substitues definition 9 in paper
-- return nothing if no create
fuzzyAngleOfVector :: FuzzyVector ->  DiscreteSpace -> FuzzyAngle
fuzzyAngleOfVector myA  space angle = maximum' . map (\v -> myA (v) ) .filter (\v-> angle == (myround 0 (calcAngleFromYAxisClockwise v) )) $ createDescriteVectorsForSpace space

createAngleList :: FuzzyVector -> DiscreteSpace -> [(Angle ,Float)]
createAngleList  myA space = map (\v -> ((myround 0 (calcAngleFromYAxisClockwise v) ) ,myA (v))  ) $ createDescriteVectorsForSpace space

myXAngleList = createAngleList myXAxis universe


-- | angle: 80.0 is missing because of discretization
testFuzzyAngleof = map (\a->  (a,fuzzyAngleOfVector myXAxis universe a) ) $ fuzzyAngleDomain universe
--stimmt


-- | definition 11
fuzzyRotation :: FuzzyVector -> DiscreteSpace -> FuzzyAngle -> FuzzyVector
fuzzyRotation myA space fuzzyAngle v= maximum' .map (\alpha -> min ((fuzzyAngle alpha )) (myA (roundVector 0 (rotateCounterClockwise alpha v)) )  )  $ fuzzyAngleDomain space

debugFuzzyRotation:: FuzzyVector -> DiscreteSpace -> FuzzyAngle -> V.Vector Float -> [(Float,V.Vector Float,V.Vector Float,Float)]
debugFuzzyRotation myA space fuzzyAngle v = map (\alpha -> (alpha,v,(roundVector 0 (rotateCounterClockwise alpha v)),min ((fuzzyAngle alpha )) (myA (roundVector 0 (rotateCounterClockwise alpha v)) ))) $ fuzzyAngleDomain space

-- | 
-- 
-- >>> always 0.0
testmyFuzzyAngle =  map (\a-> (a,myFuzzyAngle myXAxis myYAxis universe a))  [0,1..120]
-- |
-- Why are there holes? e.g: 72 - 76? Does not find a vector for these angle? Make discretisation more accurate?
-- >>> myFuzzyAngle: [(72.0,1.0),(76.0,1.0),(82.0,1.0),(83.0,1.0),(90.0,1.0)]
-- >>> myFuzzyAngle1: [(64.0,1.0),(65.0,1.0),(68.0,1.0),(69.0,1.0)] -- with: [0,1..360]
-- >>> myFuzzyAngle1 : [(68.0,1.0),(69.0,1.0)] -- with: fuzzyAngleDomain universe
testmyFuzzyAngle2 = map (\a-> (a,myFuzzyAngle1 myYAxis myXAxis universe a))  $ fuzzyAngleDomain universe

-- |
--
-- >>> []
testmyFuzzyAngle3 = filter ((>0.0). (snd)) testmyFuzzyAngle2

testmyFuzzyAngle4 = map (\a-> (a,myFuzzyAngle1 myYAxis mySpoon universe a))  $ fuzzyAngleDomain universe

debugFuzzyRot = debugFuzzyRotation normSpoon universe fuzzyAngle  (V.fromList [3.0,1.0])
  where fuzzyAngle = myFuzzyAngle1 myYAxis myXAxis universe

-- debugFuzzyRot1 = filter ((>0.0). (extractLast)) debugFuzzyRot

extractLast :: (a, b, c) -> c
extractLast (_,_,c) = c

-- |
--
-- >>> [(fromList [1.0,-4.0],1.0),(fromList [2.0,-5.0],1.0),(fromList [2.0,-4.0],1.0),(fromList [2.0,-3.0],1.0),(fromList [3.0,-4.0],1.0)]
testFuzzyRotation = map (\v -> (v,fuzzyRotation normSpoon universe fuzzyAngle v)) $ createDescriteVectorsForSpace universe
 where fuzzyAngle = myFuzzyAngle1  myYAxis myXAxis universe

rotateCounterClockwise :: Angle -> V.Vector Float -> V.Vector Float
rotateCounterClockwise alpha v = ((ccRotmatrix) N.<> v )
  where ccRotmatrix = (2N.><2) [cos radAlpha, -sin radAlpha , sin radAlpha, cos radAlpha]
        radAlpha = alpha*pi/180


rotateClockwise :: Angle -> V.Vector Float -> V.Vector Float
rotateClockwise alpha v =  ((cRotmatrix) N.<> v )
 where cRotmatrix = (2N.><2) [cos radAlpha, sin radAlpha , -sin radAlpha,cos radAlpha]
       radAlpha = alpha *pi/180

rotclock = (V.fromList [1.0,0.0]) ==  (roundVector 1 $ rotateClockwise 90  (V.fromList [0.0,1.0]) ) 
rotcounter= (V.fromList [0.0,1.0]) == (roundVector 1 $ rotateCounterClockwise 90 (V.fromList [1.0,0.0]))
rotTest = (V.fromList [1.0,0.0]) == (roundVector 1. rotateCounterClockwise 90. rotateClockwise 90 $ (V.fromList [1.0,0.0]) )

test = map (calcAngleFromYAxisClockwise) [(V.fromList [1.0,1.0]),(V.fromList [-1.0,1.0]),(V.fromList [1.0,-1.0]),(V.fromList [-1.0,-1.0])]

createDescriteUnitVectorSpace ::  [V.Vector Float]
createDescriteUnitVectorSpace  = zipWith (\a v ->(roundVector 1)$ (rotateClockwise a v) ) [0.0,2.0..360] (repeat (V.fromList [0.0,1.0]) ) 


-- | nub because many vectors have the same norm vector, e.g. [1.0,1.0], [2.0,2.0] = [0.707...,0.707..]
createDescriteUnitVectorForSpace :: DiscreteSpace -> [V.Vector Float]
createDescriteUnitVectorForSpace  space= nub . map (normVector) $ createDescriteVectorsForSpace space

testUnit = createDescriteUnitVectorForSpace universe

normVector :: V.Vector Float ->V.Vector Float
normVector v = V.map (/ norm) v
 where norm =  N.pnorm N.PNorm2 v




type FuzzyAngle = (Angle -> Probability)
-- | calculate a fuzzy angle
-- implementation of the definition 10 in the paper
{- fuzzyAngle :: FuzzyVector
                  -> FuzzyVector
                  -> DiscreteSpace
                  -> FuzzyAngle
fuzzyAngle myA myB space angle =  maximum' . map (\v1 -> min (unitmyA v1) (unitmyB (findNearestUnitVector space (rotate angle v1)) ))  $ createDescriteUnitVectorForSpace space 
 where unitmyA = fuzzyUnitVector myA space
       unitmyB = fuzzyUnitVector myB space
  -}     

{-
debugFuzzyAngle :: FuzzyVector
                -> FuzzyVector
                -> DiscreteSpace
                ->  Float
                -> [(V.Vector Float, Float, Float, Float)]

debugFuzzyAngle myA myB space angle =  map (\v1 -> (v1,(unitmyA v1), (unitmyB (findNearestUnitVector space (rotate angle v1)) ),min (unitmyA v1) (unitmyB (findNearestUnitVector space (rotate angle v1)) )))  $ createDescriteUnitVectorForSpace space 
   where unitmyA = fuzzyUnitVector myA space
         unitmyB = fuzzyUnitVector myB space


angletest= map (fuzzyAngle myFork mySpoon universe)  [0,1..180]
-}


-- vectoren nach x sortieren und dann den vector auswählen der mit x und y an nächsten sind

--t = sortBy (\(d1,_) (d2,_) -> compare2d d1 d2) $ map (\e-> (e - (V.fromList [0.99::Float,0.11]),e) ) $ createDescriteUnitVectorForSpace universe
{-
rotTest = calcAngle toRotFork toRotSpoon universe 0
rotAllAnglesTest = map (\a -> (calcAngle toRotFork toRotSpoon universe a,a)) [0,1..360]
-}



testrotate = zipWith (\a v -> (a,v,rotateClockwise a v))  [0,10..360] (repeat (V.fromList [2,3])) 
testrotat45 = rotateClockwise 45 (V.fromList [2,3])

-- runden
rtest = roundVector 2 (V.fromList [0.1,1.0])
rtes2 = roundVector 2 (V.fromList [0.5,1.0])
rtes3 = roundVector 2 (V.fromList [0.6,1.0])

roundVector :: Int -> V.Vector Float -> V.Vector Float
roundVector accuracy v = V.map (toPoint5Raster accuracy )  v

top5test = toPoint5Raster 1 0.77777 -- >>> 1.0
top5test1 = toPoint5Raster 1 0.1 -- >>> 0.0
top5test2 = toPoint5Raster 1 0.55 -- >>> 1.0

top5test22 = toPoint5Raster 2 0.77777 -- >>> 1.0
top5test12 = toPoint5Raster 2 0.1 -- >>> 0.0
top5test23 = toPoint5Raster 2 0.55 -- >>> 1.0

toPoint5Raster :: Int ->  Float -> Float
toPoint5Raster accuracy v = 
 if (roundValue - a) >= 0.5
   then roundValue + 1.0 - (roundValue - a)
   else roundValue  - (roundValue -a)
 where roundValue = myround accuracy v
       a = (myround accuracy roundValue)

-- | test the myround function
--
-- >>> [(0,0.7777778,1.0),(1,0.7777778,0.8),(2,0.7777778,0.78),(3,0.7777778,0.778),(4,0.7777778,0.7778),(5,0.7777778,0.77778),(6,0.7777778,0.777778),(7,0.7777778,0.7777778)]
mroundtest = take 8 $ zipWith (\a f-> (a,f,myround a f)) [0,1..] (repeat 0.777777777777777) 

myround :: Int -> Float -> Float
myround accuracy number =  (fromInteger $ round $ number * (10^accuracy)) / (10.0^^accuracy)

t4 = V.map (*3) (V.fromList v)      
 where v = [0.5,0.5]::[Float]


-- does not work
vecRot :: ( V.Vector Float -> Probability) ->DiscreteSpace-> Angle -> V.Vector Float -> [Float]-- [V.Vector Float]-- Probability
vecRot my s an v= map my$  filter (\n-> (roundVector 1 ( rotateClockwise an n)) == v) discSpace --maximum. map (my) $
 where discSpace = createDescriteVectorsForSpace s


testround = roundVector 1 testrotat45




       
a = V.fromList [-2:: Float,-2]
b = V.fromList [3:: Float ,1]
-- TODO

             




createDescriteVectorsForCombination :: DiscreteSpace -> [(V.Vector Float, V.Vector Float)]
createDescriteVectorsForCombination space = [(V.fromList [x,y], V.fromList [n,m]) | x<- [miX,miX+step..maX],y<-[miY,miY+step..maY], n<-[miX,miX+step..maX],m <- [miY,miY+step..maY]]
  where step = stepsize space
        miX = minX space
        miY = minY space
        maX = maxX space
        maY = maxY space

discretizeFuzzyVector ::DiscreteSpace -> FuzzyVector  -> Distribution
discretizeFuzzyVector space memFunc  = map memFunc .createDescriteVectorsForSpace $ space


createDescriteVectorsForSpace :: DiscreteSpace ->  [V.Vector Float]
createDescriteVectorsForSpace space  = [ (V.fromList [x,y]) | x<- [miX,miX+step..maX],y<-[miY,miY+step..maY]] 
 where step = stepsize space
       miX = minX space
       miY = minY space
       maX = maxX space
       maY = maxY space

-- * todo look after article and see what is can represent
conjunctionLikeSets :: ( V.Vector Float -> Probability) ->( V.Vector Float -> Probability)->DiscreteSpace -> V.Vector Float ->  V.Vector Float -> Probability
conjunctionLikeSets myA myB space xi xj =maximum .map (\(vi,vj) -> min (myA vi) (myB vj)) $ matchingVectors
 where matchingVectors = filter (\(vi,vj)-> vi+vj==xi+xj) $ createDescriteVectorsForCombination space



disjunctionLikeSets :: ( V.Vector Float -> Probability) ->( V.Vector Float -> Probability)-> DiscreteSpace -> V.Vector Float ->  V.Vector Float -> Probability
disjunctionLikeSets myA myB space xi xj=maximum. map (\(vi,vj) -> min (myA vi) (myB vj)) $ matchingVectors
 where matchingVectors = filter (\(vi,vj)-> vi-vj==xi-xj) $ createDescriteVectorsForCombination space


-- * check if needed:
-- ist my wert von einem fuzzy scale vector
-- von cripsen skalierfaktoren in probabilities mappen
-- gegeben wird lambda berechnen v2 aus und iterieren über v1
-- iterieren über alle scalefactors und v2
-- visualisierung über skalfactors
myLambda :: FuzzyVector-> FuzzyVector-> DiscreteSpace -> V.Vector Float -> FuzzyScaleFactor
myLambda myA myB space v2 scalefactor = maximum' .map (\v1 -> min (myA v1) (myB v2)) $ matchedVectors
 where matchedVectors = filter ((==v2) . roundVector 2 . (crispScale scalefactor) ) $ createDescriteVectorsForSpace space

test3 = myLambda1 myFork myFork universe 10

test4 = map (myLambda1 myFork myFork universe) [0.0,0.1..10]
-- hier ergibt es werte wenn die beiden vectoren überlappen
test5 = map (\i -> (i,myLambda1 myFork subSpoon universe i)) [0.0,0.1..10]



-- * not needed any more
findNearestUnitVector :: DiscreteSpace ->V.Vector Float ->  V.Vector Float
findNearestUnitVector space v = pointToVector $ fromJust maybeNeighbour
 where kdtree = K.fromList . map vectorToPoint $ createDescriteUnitVectorForSpace space
       maybeNeighbour =  K.nearestNeighbor kdtree $ vectorToPoint v


vectorToPoint :: V.Vector Float -> K.Point3d
vectorToPoint v = K.Point3d x1 y1 0
 where x1 = float2Double $ V.head v
       y1 = float2Double . V.head.V.drop 1 $ v

pointToVector ::  K.Point3d -> V.Vector Float
pointToVector p = V.fromList $ map (double2Float) $ zipWith K.coord [1,2] (repeat p)      

createKDTree = K.fromList $ map vectorToPoint $ createDescriteUnitVectorForSpace universe

nearest = K.nearestNeighbor createKDTree (vectorToPoint(V.fromList [0.99,0.11]))

                   

{-

vecDot :: ( V.Vector Float -> Probability) ->( V.Vector Float -> Probability)-> DiscreteSpace -> V.Vector Float-> V.Vector Float -> Angle
vecDot myA myB s vi vj =maximum .map (\(a,b) -> min (myA a) (myB b)) $ generateCombination s vi vj 
     where generateCombination space vi vj =  [(V.fromList [x,y], V.fromList [n,m]) | x<- [miX,miX+step..maX],y<-[miY,miY+step..maY], n<-[miX,miX+step..maX],m <- [miY,miY+step..maY],theta (V.fromList [x,y]) (V.fromList [n,m])==theta vi vj]
             where step = stepsize space
                   miX = minX space
                   miY = minY space
                   maX = maxX space
                   maY = maxY space
                   

dot2d :: V.Vector Float -> V.Vector Float -> Float
dot2d a b = x1*x2-y1*y2
    where x1 = V.head a
          y1 = V.head.V.drop 1 $ a
          x2 = V.head b
          y2 = V.head.V.drop 1 $ b

cross2d :: V.Vector Float -> V.Vector Float -> Float
cross2d a b = x1*y2-y1*x1
    where x1 = V.head a
          y1 = V.head.V.drop 1 $ a
          x2 = V.head b
          y2 = V.head.V.drop 1 $ b

-}
