module ReferenceFrame where

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N
n

import           Helper
import Distributions
import FuzzyVector
universe = S (-10) (-10) 10 10 1

testCenterFunction = toGnuPlotDataFile centerTest universe "centerTest.dat"
 where centerTest = center universe mySpoon mySpoon

t2 = createDescriteVectorsForSpace universe 

t3 = (val,comb)
 where v =(V.fromList [2,0])
       val = maximum' . map myFork $ comb
       comb = myCombinations universe 2 v


rotFork = (vecRot myFork universe 45) (V.fromList [0,0])

-- * initialize membership Functions
gnuScaledFork = toGnuPlotDataFile scaledFork universe fname
  where scaledFork = (scale subFork universe 2)
        fname = "ScaledsubFork.dat"

-- *  empirische Beweis, dass addition kummutativ ist
isAdditionCommutative space=discretizeFuzzyVector space (vecAdd mySpoon newFork  space) == 
                            discretizeFuzzyVector space (vecAdd newFork mySpoon space) 


-- * gnuPlot data exports

generateGnuPlotDataFileDistris=sequence_ $  zipWith3 toGnuPlotDataFile fuzzyVectors (repeat universe) fuzzyVectorNames
 where fuzzyVectors=[myFork,mySpoon,myPyramide,mySteps,subSpoon]
       fuzzyVectorNames=["Fork.dat","Spoon.dat","Pyramide.dat","Steps.dat","subSpoon.dat"]

generateGnuPlotDataFileCalc =sequence_$ zipWith3 (\v u n -> toGnuPlotDataFile (v u) u n)  fuzzyVectors (repeat universe) fuzzyVectorNames
 where fuzzyVectors = [forkAddSpoon,forkAddPyramide,spoonAddSteps,pyraAddSteps,stepsAddPyramide,forkSubSpoon,spoonSubFork]
         where forkAddSpoon = vecAdd myFork mySpoon
               forkAddPyramide = vecAdd myFork myPyramide
               spoonAddSteps= vecAdd mySpoon mySteps
               pyraAddSteps= vecAdd myPyramide mySteps
               stepsAddPyramide = vecAdd mySteps myPyramide
               forkSubSpoon = vecSub myFork mySpoon
               spoonSubFork = vecSub mySpoon myFork
       fuzzyVectorNames = ["ForkAddSpoon.dat","ForkAddPyramide.dat","SpoonAddSteps.dat","PyramideAddSteps.dat","StepsAddPyramide.dat","ForkSubSpoon.dat","SpoonSubFork.dat"]






{--


test probSpace= map (\x -> vecDot myPyramide mySteps x (V.fromList [3,3])) (fst probSpace)
testp = printDistribution test 11

-- create vectors to show the distribution functions


---

distributionPyramideAddSpoon =  zipWith (conjunctionLikeSets myPyramide mySpoon) probSpace2 probSpace2

printPyramideAddSpoon = printDistribution distributionPyramideAddSpoon 11


distributionPyramideAddSteps =  zipWith (conjunctionLikeSets myPyramide mySteps) probSpace2 probSpace2
distributionPyramideSubSteps =  zipWith (disjunctionLikeSets myPyramide mySteps) probSpace2 probSpace2
printPyramideAddSteps = printDistribution distributionPyramideAddSteps 11
printPyramideSubSteps = printDistribution distributionPyramideSubSteps 11
-- other resuolution

-- | TODO
vecDot :: ( V.Vector Float -> Probability) ->( V.Vector Float -> Probability)-> V.Vector Float ->  V.Vector Float -> Probability
vecDot myA myB xi xj = (N.dot xi xj) -- * vecAdd myA myB xi xj

distributionForkConjunctionSpoon = zipWith (conjunctionLikeSets myFork mySpoon) probSpace2 probSpace2
distributionForkDisjunctionSpoon= zipWith (disjunctionLikeSets myFork mySpoon) probSpace2 probSpace2

printForkConjSpoon = printDistribution distributionForkConjunctionSpoon 11
printForkDisjSpoon = printDistribution distributionForkDisjunctionSpoon 11
--}

