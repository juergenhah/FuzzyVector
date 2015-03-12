{-|
Module      : Discretization
Description : Implements functions to discretize a space for fuzzy vectors
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

Implementation of discretization functions 
-}
module Discretization ( DiscreteSpace (..)
                      , createVectorDomain
                      , createVectorsForBinaryOps
                      , fitToVectorDomain
                      , createScaleDomain
                      , fitToScaleDomain
                      , createAngleDomain
                      , fitToAngleDomain
                      ) where

import           Data.Fixed
import           Data.List
import qualified Data.Vector.Storable as V
import           Control.Parallel.Strategies
import           Control.DeepSeq

import           CrispVector

-- | defines the discretization bounding rectangle and the granularity
data DiscreteSpace =S {minX:: Double
                     , minY:: Double
                     , maxX:: Double
                     , maxY:: Double
                     , stepsize::Double} deriving (Show, Read)

-- * function for the discrete vector  domain

-- | creates a list of crisp vectors for the descrite space
-- starting from min to max with a stepsize given by discretespace
createVectorDomain :: DiscreteSpace -- ^ discrete space to work on
                   -> [CrispVector] -- ^ list of crisp vector for the discrete space
createVectorDomain space  = [ (crispVector x y) | x<- [miX,miX+step..maX]
                                                , y<- [miY,miY+step..maY]
                            ] 
 where step = stepsize space
       miX = minX space
       miY = minY space
       maX = maxX space
       maY = maxY space

-- | creates all vector possible combinations for the discrete space
createVectorsForBinaryOps' :: DiscreteSpace                -- ^ discrete space 
                           -> [(CrispVector, CrispVector)] -- ^ all possible combinations of crisp vectors for the discrete space
createVectorsForBinaryOps' space = [ (crispVector x y, crispVector n m) 
                                               | x<- [miX,miX+step..maX]
                                               , y<- [miY,miY+step..maY]
                                               , n<- [miX,miX+step..maX]
                                               , m <- [miY,miY+step..maY]
                                   ]
  where step = stepsize space
        miX = minX space
        miY = minY space
        maX = maxX space
        maY = maxY space

-- | performant implementation of createDescriteVectorsForBinaryOps 
createVectorsForBinaryOps :: DiscreteSpace                 -- ^ discrete space
                          -> [(CrispVector, CrispVector)]  -- ^ all possible vector combinations       
createVectorsForBinaryOps space  = runEval $ 
 do 
 let step = stepsize space
     miX = minX space
     miY = minY space
     maX = maxX space
     maY = maxY space
     sol = runEval $ do 
       let fstartX = step
           fstartY = step
       first <-rpar (force ([ (crispVector x y, crispVector n m ) 
                              | x <- [fstartX, fstartX+step..maX]
                              , y <- [fstartY, fstartY+step..maY]
                              , n <- [miX, miX+step..maX]
                              , m <- [miY, miY+step..maY]
                            ] ))
       let sstartX = miX
           sstartY = step
       second <-rpar (force ([ (crispVector x y, crispVector n m) 
                               | x <- [sstartX, sstartX+step..0]
                               , y <- [sstartY, sstartY+step..maY]
                               , n <- [miX, miX+step..maX]
                               , m <- [miY, miY+step..maY]
                             ] ))
       third <-rpar (force ([ (crispVector x y, crispVector n m) 
                              | x <- [miX, miX+step..0]
                              , y <- [miY, miY+step..0]
                              , n <- [miX, miX+step..maY]
                              , m <- [miY, miY+step..maY]
                             ] ))
       let fstartX = step
       fourth <-rpar (force ([ (crispVector x y ,crispVector n m)  
                               | x <- [fstartX, fstartX+step..maX]
                               , y <- [miY, miY+step..0]
                               , n <- [miX, miX+step..maX]
                               , m <- [miY, miY+step..maY]
                              ] ))
       rseq first
       rseq second
       rseq third
       rseq fourth
       return (first++second++third++fourth)
 return sol



-- ** functions to map crisp vectors to a discrete space

-- | rounds a crisp vector having only one decimal position
-- this implementation fits to 0.5 values only
fitToVectorDomain :: CrispVector -- ^ general crisp vector
                       -> CrispVector -- ^ crisp vector round to the vector domain
fitToVectorDomain = fitEveryPosToP5Space 0

-- | fits ever crisp vector to a 0.5 raster
fitEveryPosToP5Space :: Int         -- ^ accuracy, how many positions after comma
                     -> CrispVector -- ^ vector to round
                     -> CrispVector -- ^ rounded vector
fitEveryPosToP5Space accuracy (C v) = C (V.map (toPoint5Raster accuracy )  v) 

-- | rounds a value to a 0.5 raster [0.0,0.5,1.0,...]
toPoint5Raster :: Int    -- ^ accuracy
               -> Double -- ^ value to round
               -> Double -- ^ rounded value
toPoint5Raster accuracy v = 
 if (roundValue - a) >= 0.5
   then roundValue + 1.0 - (roundValue - a)
   else roundValue - (roundValue -a)
 where roundValue = roundtoFractionalDigits  accuracy v
       a = roundtoFractionalDigits accuracy roundValue

-- | rounds with given accuracy
roundtoFractionalDigits :: Int    -- ^ accuray, how many positions after comma
                        -> Double -- ^ value to round
                        -> Double -- ^ rounded value
roundtoFractionalDigits amountFractionalDigits number =  (fromInteger $ round $ number * (10^amountFractionalDigits)) / (10.0^^amountFractionalDigits)


-- * functions for scale domain

-- | list including all scale factors we want to look at 
createScaleDomain :: [Double] -- ^ list with scale factors
createScaleDomain = map (roundtoFractionalDigits 2) [0.0,0.1..10] 

-- | fits an scaling factor to the scaling domain
-- rounds to 2 decimal positions
fitToScaleDomain :: Double -- ^ value to round
                 -> Double -- ^ rounded value
fitToScaleDomain = roundtoFractionalDigits 1


-- * functions for angle domain

-- | list including all angles for the vectors the discrete space has
createAngleDomain :: DiscreteSpace -- ^ discrete space 
                  -> [Angle]       -- ^ list of angles for this space
createAngleDomain space = nub. map (fitToAngleDomain .calcAngleFromYAxisClockwise) $ createVectorDomain space

-- | fits an angle to the angle domain
-- domain range: [0,1..360]
fitToAngleDomain :: Angle -- ^ value to round
                 -> Angle -- ^ rounded and trimmed value
fitToAngleDomain alpha =roundtoFractionalDigits 0 $ mod' alpha 360



