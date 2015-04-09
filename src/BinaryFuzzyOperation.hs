{-# LANGUAGE BangPatterns #-}
{-|
Module      : BinaryFuzzyOperation
Description : Implements binary operations for fuzzy vectors
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

Implementation of the binary operations for fuzzy vectors
-}
module BinaryFuzzyOperation ( fuzzyIntersectionMap
                            , fuzzyIntersection
                            , fuzzyUnionMap
                            , fuzzyUnion
                            , fuzzyAdd
                            , fuzzySub
                            , center
                            , scaleAasBtoC
                            , fuzzyComplement
                            ) where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

import CrispVector
import Discretization
import FuzzyVector

-- | general type of a binary function on two crisp vectors
type BinaryVectorOperation = CrispVector -> CrispVector -> CrispVector

-- | intersects two fuzzy vectors
fuzzyIntersection :: FuzzyVector -- ^ first fuzzy vector to intersect
                   -> FuzzyVector -- ^ second fuzzy vector to intersect
                   -> FuzzyVector -- ^ intersected fuzzy vector
fuzzyIntersection muA muB v = min (muA v) (muB v)

fuzzyIntersectionMap :: FuzzyMap -- ^ first fuzzy vector to intersect
                      -> FuzzyMap -- ^ second fuzzy vector to intersect
                      -> FuzzyMap -- ^ intersected fuzzy vector
fuzzyIntersectionMap muA muB  = Map.unionWith min muA muB

-- | unions two fuzzy vectors
fuzzyUnionMap :: FuzzyMap -- ^ first fuzzy vector for union
              -> FuzzyMap -- ^ second fuzzy vector for union
              -> FuzzyMap -- ^ resulting fuzzy vector of the union
fuzzyUnionMap muA muB  = Map.unionWith max muA muB

fuzzyUnion  :: FuzzyVector -- ^ first fuzzy vector to intersect
             -> FuzzyVector -- ^ second fuzzy vector to intersect
             -> FuzzyVector -- ^ intersected fuzzy vector
fuzzyUnion muA muB v = max (muA v) (muB v)

-- | calculates the complement of one fuzzy vector against the second one
fuzzyComplement :: FuzzyVector -- ^ first fuzzy vector
                    -> FuzzyVector -- ^ second fuzzy vector
                    -> FuzzyVector -- ^ complemented vector
fuzzyComplement muA muB v
 | (muA v - muB v) <0.0 = 0.0
 | otherwise = muA v - muB v

-- | general binary fuzzy operation
binaryFuzzyOperation :: DiscreteSpace           -- ^ discrete space to work on
                      -> FuzzyMap            -- ^ first fuzzy vector       
                      -> FuzzyMap            -- ^ second fuzzy vector
                      -> BinaryVectorOperation  -- ^ function that operates on two crisp vectors
                      -> FuzzyMap            -- ^ resulting fuzzy vector
binaryFuzzyOperation !space !muA !muB operation = Map.mapWithKey (\k _ -> maximum' $ runEval $ evalSpaceParallel  space muA muB operation k) newMap
  where newMap = Map.fromList allVecs
        allVecs = map (\v -> (v, 0.0::MemberShipValue)) . createVectorDomain $ space

binResultForVect :: DiscreteSpace -> FuzzyMap -> FuzzyMap 
 ->BinaryVectorOperation ->CrispVector-> MemberShipValue
binResultForVect space muA muB operation x = maximum'. map (\(xi,xj) -> min (getMemberShipValue muA xi) (getMemberShipValue muB xj)) $! matchingVectors
 where !matchingVectors = filter (\(xi,xj) -> ( xi `operation` xj) ==x) $! createVectorsForBinaryOps  space

{-
binaryLookupTable ::  DiscreteSpace         -- ^ discrete space to work on
                  -> FuzzyVector           -- ^ first fuzzy vector
                  -> FuzzyVector           -- ^ second fuzzy vector
                  -> BinaryVectorOperation -- ^ function that operates on two
                  -> [(CrispVector,MemberShipValue)]
binaryLookupTable !space !muA !muB !operation = map (\v-> (v, binaryFuzzyOperation' space muA muB operation v)) $! createVectorDomain space
-}

-- | performs an binary fuzzy operation in parallel
-- the discrete space is split in four parts accordin to coordinate system quadrants
evalSpaceParallel :: DiscreteSpace          -- ^ discrete space to work on
                  -> FuzzyMap            -- ^ first fuzzy vector
                  -> FuzzyMap            -- ^ second fuzzy vector
                  -> BinaryVectorOperation  -- ^ function that operates on two crisp vectors
                  -> CrispVector            -- ^ actual vector for this calculation 
                  -> Eval [MemberShipValue] -- ^ membership value for the actual crisp value
evalSpaceParallel !space muA muB operation v= do
 let step = stepsize space
     miX = minX space
     miY = minY space
     maX = maxX space
     maY = maxY space
     sol = runEval $! do 
      let fstartX = 0+step
          fstartY = 0+step
      first <-rpar (force ([min (getMemberShipValue muA (crispVector x y)) 
                                (getMemberShipValue muB (crispVector n m)) 
                           | x <- [fstartX,fstartX+step..maX]
                           , y <- [fstartY,fstartY+step..maY]
                           , n <- [miX,miX+step..maX]
                           , m <- [miY,miY+step..maY]
                           , ((crispVector x y) `operation` (crispVector n m))  == v
                           ] ))
      let sstartX = miX
          sstartY = 0+step
      second <-rpar (force ([min (getMemberShipValue muA (crispVector x y)) 
                                 (getMemberShipValue muB (crispVector n m)) 
                            | x <- [sstartX,sstartX+step..0]
                            , y <- [sstartY,sstartY+step..maY]
                            , n <- [miX,miX+step..maX]
                            , m <- [miY,miY+step..maY]
                            , (crispVector x y) `operation` (crispVector n m) == v
                            ] ))
      third <-rpar (force ([min (getMemberShipValue muA (crispVector x y)) 
                                (getMemberShipValue muB (crispVector n m)) 
                           | x <- [miX,miX+step..0]
                           , y <- [miY,miY+step..0]
                           , n <- [miX,miX+step..maY]
                           , m <- [miY,miY+step..maY]
                           , (crispVector x y) `operation` (crispVector n m) == v
                           ] ))
      let fstartX = 0+step
      fourth <-rpar (force ([min (getMemberShipValue muA (crispVector x y)) 
                                 (getMemberShipValue muB (crispVector n m)) 
                            | x <- [fstartX,fstartX+step..maX]
                            , y <- [miY,miY+step..0]
                            , n <- [miX,miX+step..maX]
                            , m <- [miY,miY+step..maY]
                            , (crispVector x y) `operation` (crispVector n m) == v
                            ] ))
      rseq first
      rseq second
      rseq third
      rseq fourth
      return (first++second++third++fourth)
 return sol

-- | calculates the membership value of the summed fuzzy vectors for one crisp input vector v
fuzzyAdd :: DiscreteSpace -- ^ discrete space to work on
         -> FuzzyMap   -- ^ first fuzzy vector 
         -> FuzzyMap   -- ^ second fuzzy vector
         -> FuzzyMap   -- ^ added fuzzy vector 
fuzzyAdd space muA muB  = binaryFuzzyOperation space muA muB (add)


-- | calculates the membership value of the summed  fuzzy vectors for one crisp input vector v
fuzzySub :: DiscreteSpace -- ^ discrete space to work on
         -> FuzzyMap   -- ^ first fuzzy vector 
         -> FuzzyMap   -- ^ second fuzzy vector 
         -> FuzzyMap   -- ^ subtracted fuzzy vector
fuzzySub space muA muB = binaryFuzzyOperation space muA muB (sub) 


-- | translates a fuzzy vector to the origin of the discrete space
center :: DiscreteSpace -- ^ discrete space to work on 
       -> FuzzyMap   -- ^ fuzzy vector to center
       -> FuzzyMap   -- ^ centered fuzzy vector
center space muA = fuzzySub space muA muA 



scaleAasBtoC :: DiscreteSpace -- ^ discrete space to work on
               -> FuzzyMap   -- ^ first fuzzy vector, used to calculate scale factor
               -> FuzzyMap   -- ^ second fuzzy vector, used to calculate scale factor
               -> FuzzyMap   -- ^ fuzzy vector to scale
               -> FuzzyMap   -- ^ scaled fuzzy vector
scaleAasBtoC !space muB muC muA  = crispScale muA sf 
 where sf = (calcCentroidFuzzyScaleFactor space scalefactor) --traceShow "scaleFactor"
       scalefactor = calculateFuzzyScaleFactor space centermuB centermuC 
       centermuB = translateToCenterWithCentroid space muB
       centermuC = translateToCenterWithCentroid space muC
{-
-- | scales the third fuzzy vector with a scaling factor calculated from the first and second fuzzy vector
scaleAasBtoC2 :: DiscreteSpace -- ^ discrete space to work on
               -> FuzzyVector   -- ^ first fuzzy vector, used to calculate scale factor
               -> FuzzyVector   -- ^ second fuzzy vector, used to calculate scale factor
               -> FuzzyVector   -- ^ fuzzy vector to scale
               -> FuzzyVector   -- ^ scaled fuzzy vector
scaleAasBtoC2 space muB muC muA v = crispScale muA sf v   
 where !sf =calcCentroidFuzzyScaleFactor space scalefactor
       !scalefactor = scaleAasBtoC' space muB muC muA 

scaleAasBtoC1 :: DiscreteSpace -- ^ discrete space to work on
               -> FuzzyVector   -- ^ first fuzzy vector, used to calculate scale factor
               -> FuzzyVector   -- ^ second fuzzy vector, used to calculate scale factor
               -> FuzzyVector   -- ^ fuzzy vector to scale
               -> FuzzyVector   -- ^ scaled fuzzy vector
scaleAasBtoC1 space muB muC muA v = crispScale muA sf v   
 where !sf =calcCentroidFuzzyScaleFactor space scalefactor
       !scalefactor = scaleAasBtoC' space muB muC muA 

-- | performant implementation of scaleAasBtoC
scaleAasBtoC' :: DiscreteSpace    -- ^ discrete space to work on
              -> FuzzyVector      -- ^ first fuzzy vector, used to calculate scale factor
              -> FuzzyVector      -- ^ second fuzzy vector, used to calculate scale factor
              -> FuzzyVector      -- ^ fuzzy vector to scale
              -> FuzzyScaleFactor -- ^ fuzzy scale factor for actual fuzzy vector
scaleAasBtoC'  space muB muC muA = do
  sol <- runEval $! do
   centermuB <- rpar (force (translateToCenterWithCentroid space muB ))
   centermuC <- rpar (force (translateToCenterWithCentroid space muC ))
   rseq centermuB
   rseq centermuC
   scalefac <- rpar (force (calculateFuzzyScaleFactor space centermuB centermuC))
   rseq scalefac
   return scalefac
  return sol

-}
