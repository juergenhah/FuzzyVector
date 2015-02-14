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
module BinaryFuzzyOperation ( fuzzyIntersection
                            , fuzzyUnion
                            , fuzzyAdd
                            , fuzzySub
                            , center
                            , scaleAasBtoC
                            ) where

import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Data.List

import           CrispVector
import           Discretization
import           FuzzyVector

-- | general type of a binary function on two crisp vectors
type BinaryVectorOperation = CrispVector -> CrispVector -> CrispVector

-- | intersects two fuzzy vectors
fuzzyIntersection :: FuzzyVector -- ^ first fuzzy vector to intersect
                  -> FuzzyVector -- ^ second fuzzy vector to intersect
                  -> FuzzyVector -- ^ intersected fuzzy vector
fuzzyIntersection muA muB v = min (muA v) (muB v)

-- | unions two fuzzy vectors
fuzzyUnion :: FuzzyVector -- ^ first fuzzy vector for union
           -> FuzzyVector -- ^ second fuzzy vector for union
           -> FuzzyVector -- ^ resulting fuzzy vector of the union
fuzzyUnion muA muB v = max (muA v) (muB v)

-- | general binary fuzzy operation
binaryFuzzyOperation :: DiscreteSpace           -- ^ discrete space to work on
                      -> FuzzyVector            -- ^ first fuzzy vector       
                      -> FuzzyVector            -- ^ second fuzzy vector
                      -> BinaryVectorOperation  -- ^ function that operates on two crisp vectors
                      -> FuzzyVector            -- ^ resulting fuzzy vector
binaryFuzzyOperation space muA muB operation x = maximum'. map (\(xi,xj) -> min (muA xi) (muB xj)) $! matchingVectors
 where matchingVectors = filter (\(xi,xj) -> ( xi `operation` xj) ==x) $! createVectorsForBinaryOps  space

-- | performant implementation for a binary fuzzy operation
binaryFuzzyOperation' :: DiscreteSpace         -- ^ discrete space to work on
                      -> FuzzyVector           -- ^ first fuzzy vector
                      -> FuzzyVector           -- ^ second fuzzy vector
                      -> BinaryVectorOperation -- ^ function that operates on two crisp vectors
                      -> FuzzyVector           -- ^ resulting fuzzy vector
binaryFuzzyOperation' space muA muB operation x = maximum' matchingVectors
  where matchingVectors =runEval $! evalSpaceParallel space muA muB operation x

-- | performs an binary fuzzy operation in parallel
-- the discrete space is split in four parts accordin to coordinate system quadrants
evalSpaceParallel :: DiscreteSpace          -- ^ discrete space to work on
                  -> FuzzyVector            -- ^ first fuzzy vector
                  -> FuzzyVector            -- ^ second fuzzy vector
                  -> BinaryVectorOperation  -- ^ function that operates on two crisp vectors
                  -> CrispVector            -- ^ actual vector for this calculation 
                  -> Eval [MemberShipValue] -- ^ membership value for the actual crisp value
evalSpaceParallel space muA muB  operation v= do
 let step = stepsize space
     miX = minX space
     miY = minY space
     maX = maxX space
     maY = maxY space
     sol = runEval $ do 
      let fstartX = 0+step
          fstartY = 0+step
      first <-rpar (force ([min (muA (crispVector x y)) 
                                (muB (crispVector n m)) 
                           | x <- [fstartX,fstartX+step..maX]
                           , y <- [fstartY,fstartY+step..maY]
                           , n <- [miX,miX+step..maX]
                           , m <- [miY,miY+step..maY]
                           , ((crispVector x y) `operation` (crispVector n m))  == v
                           ] ))
      let sstartX = miX
          sstartY = 0+step
      second <-rpar (force ([min (muA (crispVector x y)) 
                                 (muB (crispVector n m)) 
                            | x <- [sstartX,sstartX+step..0]
                            , y <- [sstartY,sstartY+step..maY]
                            , n <- [miX,miX+step..maX]
                            , m <- [miY,miY+step..maY]
                            , (crispVector x y) `operation` (crispVector n m) == v
                            ] ))
      third <-rpar (force ([min (muA (crispVector x y)) 
                                (muB (crispVector n m)) 
                           | x <- [miX,miX+step..0]
                           , y <- [miY,miY+step..0]
                           , n <- [miX,miX+step..maY]
                           , m <- [miY,miY+step..maY]
                           , (crispVector x y) `operation` (crispVector n m) == v
                           ] ))
      let fstartX = 0+step
      fourth <-rpar (force ([min (muA (crispVector x y)) 
                                 (muB (crispVector n m)) 
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
         -> FuzzyVector   -- ^ first fuzzy vector 
         -> FuzzyVector   -- ^ second fuzzy vector
         -> FuzzyVector   -- ^ added fuzzy vector 
fuzzyAdd space muA muB  v = binaryFuzzyOperation' space muA muB  (+) v

-- | calculates the membership value of the summed  fuzzy vectors for one crisp input vector v
fuzzySub :: DiscreteSpace -- ^ discrete space to work on
         -> FuzzyVector   -- ^ first fuzzy vector 
         -> FuzzyVector   -- ^ second fuzzy vector 
         -> FuzzyVector   -- ^ subtracted fuzzy vector
fuzzySub space muA muB  v= binaryFuzzyOperation' space muA muB  (-) v           


-- | translates a fuzzy vector to the origin of the discrete space
center :: DiscreteSpace -- ^ discrete space to work on 
       -> FuzzyVector   -- ^ fuzzy vector to center
       -> FuzzyVector   -- ^ centered fuzzy vector
center space muA = fuzzySub space muA muA 

-- | scales the third fuzzy vector with a scaling factor calculated from the first and second fuzzy vector
scaleAasBtoC :: DiscreteSpace -- ^ discrete space to work on
             -> FuzzyVector   -- ^ first fuzzy vector, used to calculate scale factor
             -> FuzzyVector   -- ^ second fuzzy vector, used to calculate scale factor
             -> FuzzyVector   -- ^ fuzzy vector to scale
             -> FuzzyVector   -- ^ scaled fuzzy vector
scaleAasBtoC space muB muC muA v = fuzzyCentroidScale space muA scalefactor  v 
 where scalefactor = scaleAasBtoC' space muB muC muA v

-- | performant implementation of scaleAasBtoC
scaleAasBtoC' :: DiscreteSpace    -- ^ discrete space to work on
              -> FuzzyVector      -- ^ first fuzzy vector, used to calculate scale factor
              -> FuzzyVector      -- ^ second fuzzy vector, used to calculate scale factor
              -> FuzzyVector      -- ^ fuzzy vector to scale
              -> CrispVector      -- ^ actual fuzzy vector
              -> FuzzyScaleFactor -- ^ fuzzy scale factor for actual fuzzy vector
scaleAasBtoC'  space muB muC muA v= do
  sol <- runEval $ do
   centermuB <- rpar (force (translateToCenterWithCentroid space muB ))
   centermuC <- rpar (force (translateToCenterWithCentroid space muC ))
   rseq centermuB
   rseq centermuC
   scalefac <- rpar (force (createFuzzyScaleFactor space centermuB centermuC))
   rseq scalefac
   return $! scalefac
  return sol

{-
fuzzyComplement :: FuzzyVector -- ^
                -> FuzzyVector -- ^
                -> FuzzyVector -- ^
fuzzyComplement muA muB v
 | (muA v - muB v) <0.0 = 0
 | otherwise = muA v - muB v
-}
