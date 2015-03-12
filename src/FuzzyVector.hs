{-# LANGUAGE BangPatterns #-}
{-|
Module      : FuzzyVector
Description : Implements rotation and scale operations for fuzzy vectors
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

Implementation of rotation and scale operations for fuzzy vectors
-}
module FuzzyVector ( MemberShipValue
                   , FuzzyVector
                   , ScaleFactor
                   , FuzzyScaleFactor
                   , FuzzyAngle
                   , fuzzyThresholdTest
                   , calculateFuzzyScaleFactor
                   , calculateFuzzyScaleFactor'
                   , calcCentroidFuzzyScaleFactor
                   , lookupScale
                   , fuzzyScale
                   , crispScale
                   , fuzzyCentroidScale
                   , calculateFuzzyAngle
                   , calculateFuzzyAngle'
                   , fuzzyAngleofFuzzyVector
                   , calcCentroidAngle
                   , fuzzyRotation
                   , crispRotation
                   , fuzzyCentroidRotation
                   , translateToCenterWithCentroid
                   , crispTranslate
                   , maximum'
                   , debugCreateFuzzyScaleFactor 
                   , toFuzzyMap
                   , FuzzyMap
                   , calcCentroid
                   , getMemberShipValue 
                   ) where

import           Data.List
import qualified Data.Map as Map
import           Data.Maybe


import qualified CrispVector as C
import qualified Discretization as D

-- | datatype for fuzzy membership value 
type MemberShipValue = Double

-- | definition of a fuzzy vector
type FuzzyVector = (C.CrispVector -> MemberShipValue) 
type FuzzyMap = Map.Map C.CrispVector MemberShipValue

-- | datatype for scale factor
type ScaleFactor = Double

-- | definition of a fuzzy scale factor
type FuzzyScaleFactor = (ScaleFactor -> MemberShipValue)

-- | definition of a fuzzy angle
type FuzzyAngle = (C.Angle -> MemberShipValue)

getMemberShipValue :: FuzzyMap -> C.CrispVector -> MemberShipValue
getMemberShipValue muA v = Map.findWithDefault (0.0)  v muA

toFuzzyMap :: D.DiscreteSpace -> FuzzyVector -> FuzzyMap
toFuzzyMap space mu = Map.fromList $! map (\v -> (v,mu v)) $ D.createVectorDomain space
-- | has the fuzzy vector a membership value higher than the double value?
fuzzyThresholdTest :: D.DiscreteSpace -- ^ space to work on
                   -> FuzzyMap     -- ^ fuzzy vector to test
                   -> Double          -- ^ threshold value to be tested
                   -> Bool            -- ^ is any fuzzy membership value higher than the threshold value
fuzzyThresholdTest space muA n = Map.empty /= Map.filter (>=n) muA


-- * fuzzy Scaling

-- | scales the second fuzzy vector to fit the first fuzzy vector and returns this values
calculateFuzzyScaleFactor :: D.DiscreteSpace  -- ^ space to work on 
                          -> FuzzyMap      -- ^ first fuzzy vector
                          -> FuzzyMap      -- ^ fuzzy vector to be scaled
                          -> FuzzyScaleFactor -- ^ one dimensional  fuzzy value that shows how well the second fuzzy vector meets the first one
calculateFuzzyScaleFactor space muA muB factor=lookupScale scaleMap factor
 where !scaleMap = calculateFuzzyScaleFactor' space muA muB

-- |  creates a lookup list for fuzzy scale factors
calculateFuzzyScaleFactor' :: D.DiscreteSpace                  -- ^ space to work on 
                           -> FuzzyMap                      -- ^ first fuzzy vector
                           -> FuzzyMap                      -- ^ second fuzzy vector
                           -> [(ScaleFactor, MemberShipValue)] -- ^ lookup list
calculateFuzzyScaleFactor' space muA muB = map 
 (\lambda -> (lambda, createFuzzyScaleFactor space muA muB lambda)) $ D.createScaleDomain  


-- | lookup function
lookupScale :: [(ScaleFactor, MemberShipValue)] -- ^ lookup list
            -> FuzzyScaleFactor                 -- ^ fuzzy membership value for a searched scale factor
lookupScale !lookupList fuzzyFactor  = fromMaybe (0.0) $! lookup fuzzyFactor lookupList


-- | scales the second fuzzy vector to meet the first fuzzy vector
createFuzzyScaleFactor :: D.DiscreteSpace  -- ^ space to work on
                       -> FuzzyMap      -- ^ fuzzy vector to meet
                       -> FuzzyMap      -- ^ fuzzy vector to scale
                       -> FuzzyScaleFactor -- ^ fuzzy value that shows how well the second fuzzy vector meets the first one
createFuzzyScaleFactor space muA muB lambda = mean . map  
 (\vi -> min (getMemberShipValue muA vi) 
             (getMemberShipValue muB (D.fitToVectorDomain (C.scaleCrispVector lambda vi))) --D.trimtoDiscreteRaster 1
 ) . filter (/= C.crispVector 0.0 0.0) $! D.createVectorDomain space

debugCreateFuzzyScaleFactor  :: D.DiscreteSpace  -- ^ space to work on
                             -> FuzzyVector      -- ^ fuzzy vector to meet
                             -> FuzzyVector      -- ^ fuzzy vector to scale
                             -> ScaleFactor
                             -> [(C.CrispVector,ScaleFactor,MemberShipValue,C.CrispVector,MemberShipValue,MemberShipValue)]  -- ^ fuzzy value that shows how well the second fuzzy vector meets the first one
debugCreateFuzzyScaleFactor space !muA !muB lambda =  map  
   (\vi -> ( vi
            ,lambda
            ,muA vi
            , D.fitToVectorDomain (C.scaleCrispVector lambda vi)
            ,muB (D.fitToVectorDomain (C.scaleCrispVector lambda vi))
            ,min (muA vi) 
                 (muB (D.fitToVectorDomain (C.scaleCrispVector lambda vi))) 
            )  --D.trimtoDiscreteRaster 1
  ) . filter (/= C.crispVector 0.0 0.0) $! D.createVectorDomain space

-- | calculates the mean value of a list of values
mean :: Floating a => [a] -- ^ list of values
                    -> a  -- ^ mean value of the list
mean x = fst $! foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x


-- | scales the fuzzy vector with the previous calculated fuzzy scale factor
fuzzyScale :: FuzzyMap      -- ^ fuzzy vector to scale
           -> FuzzyScaleFactor -- ^ fuzzy scale factor to scale the fuzzy vector with
           -> FuzzyVector      -- ^ scaled fuzzy vector
fuzzyScale muA muLambda v= maximum' . map 
 (\lambda-> min (getMemberShipValue muA (D.fitToVectorDomain (C.scaleCrispVector (1/lambda) v))) 
                (muLambda lambda)
 ) .filter (/= 0.0) $! D.createScaleDomain 

-- | scales a fuzzy vector with a fixed scale factor
crispScale :: FuzzyMap -- ^ fuzzy vector to be scaled
           -> ScaleFactor -- ^ fixed scale factor
           -> FuzzyMap -- ^ scaled fuzzy vector
crispScale  mu scaleFactor = Map.fromList $! map (\(v,m)-> (v, getMemberShipValue mu (D.fitToVectorDomain $ C.scaleCrispVector (1/scaleFactor) v))) list
 where list = Map.toList mu
-- | scales a fuzzy vector with the calculated centroid of a fuzzy scale factor
fuzzyCentroidScale :: D.DiscreteSpace  -- ^ space to work on
                   -> FuzzyMap      -- ^ fuzzy vector to be scaled
                   -> FuzzyScaleFactor -- ^ fuzzy scale factor used to calculate the centroid fixed scale value
                   -> FuzzyMap      -- ^ scaled fuzzy vector
fuzzyCentroidScale space muA muLambda = crispScale muA centroidScale
 where !centroidScale = calcCentroidFuzzyScaleFactor space muLambda

-- | calculates the centroid of fuzzy scale factor
calcCentroidFuzzyScaleFactor :: D.DiscreteSpace  -- ^ space to work on
                             -> FuzzyScaleFactor -- ^ fuzzy scale factor where centroid is calculated
                             -> ScaleFactor      -- ^ fixed scale factor, centroid of the fuzzy scale factor
calcCentroidFuzzyScaleFactor space mu  =  (/m) . sum . map (\l-> (mu l)*l) 
                                                $! D.createScaleDomain
 where !m = sum. map mu $ D.createScaleDomain                  


-- | maximum of a list of doubles, for an empty list 0.0 is returned
maximum' :: [Double] -- ^ list of doubles
         -> Double   -- ^ maximum value, zero for empty list
maximum' [] = 0.0  -- if no values are found in domain changed compared to Prelude.maximum   
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

-- | debug function of fuzzy scale function
debugFuzzyScale :: FuzzyVector       -- ^ 
                -> FuzzyScaleFactor  -- ^
                -> C.CrispVector     -- ^
                -> [(ScaleFactor, C.CrispVector,C.CrispVector,MemberShipValue)]
debugFuzzyScale muA muLambda v= map (\lambda-> 
 ( lambda
 , v
 , (D.fitToVectorDomain (C.scaleCrispVector (1/lambda) v))
 , min (muA (D.fitToVectorDomain (C.scaleCrispVector (1/lambda) v))) 
       (muLambda lambda) 
 )
                                    ) . filter (/= 0.0) $! D.createScaleDomain

-- * rotate functions

-- | calculates a fuzzy angle between two fuzzy vectors
calculateFuzzyAngle :: D.DiscreteSpace -- ^ space to work on
                    -> FuzzyMap     -- ^ first fuzzy vector
                    -> FuzzyMap     -- ^ second fuzzy vector
                    -> FuzzyAngle      -- ^ fuzzy angle between the two fuzzy vectors
calculateFuzzyAngle space muA muB angle = maximum' . 
 map  (\alpha -> min (lookupFuzzyAngle lookupListmuA alpha)  
                     (lookupFuzzyAngle lookupListmuB (D.fitToAngleDomain (alpha+angle)) ) 
      )  $! D.createAngleDomain space
  where !lookupListmuA = calculateFuzzyAngle' space muA 
        !lookupListmuB = calculateFuzzyAngle' space muB

-- | calculates the fuzzy angle of an fuzzy vector regarding the positive y axis
calculateFuzzyAngle' :: D.DiscreteSpace              -- ^ space to work on
                     -> FuzzyMap                  -- ^ fuzzy vector to be measured
                     -> [(C.Angle ,MemberShipValue)] -- ^ lookup list
calculateFuzzyAngle' space muA  = Map.toList . Map.fromListWith (max) $! map (\(k,v)-> (D.fitToAngleDomain .C.calcAngleFromYAxisClockwise $ k, v) ) $Map.toList muA

-- | lookup list to fuzzy angle
-- if no value is found membership value 0.0 is returned
lookupFuzzyAngle :: [(C.Angle, MemberShipValue)] -- ^ calculated lookup list
                 -> FuzzyAngle                   -- ^ fuzzy angle to be searched for
lookupFuzzyAngle lookupList angle= fromMaybe (0.0) $! lookup angle lookupList

-- | calculate the angles from positive Y axis to the fuzzy vector
fuzzyAngleofFuzzyVector :: D.DiscreteSpace -- ^ space to work on
                        -> FuzzyMap     -- ^ measure angles from this fuzzy vector 
                        -> FuzzyAngle      -- ^ fuzzy angle of this fuzzy vector
fuzzyAngleofFuzzyVector space mu= lookupFuzzyAngle $! calculateFuzzyAngle' space mu   


-- | rotate a fuzzy vector with a precalculated fuzzy vector
fuzzyRotation :: D.DiscreteSpace -- ^ space to work on
              -> FuzzyMap     -- ^ fuzzy vector to rotate
              -> FuzzyAngle      -- ^ fuzzy angle to rotate with
              -> FuzzyMap     -- ^ rotated fuzzy vector
fuzzyRotation space muA fuzzyAngle = Map.mapWithKey (\k _ -> rotOpt space muA fuzzyAngle k) newMap 
 where newMap = Map.fromList allVecs
       allVecs = map (\v -> (v, 0.0::MemberShipValue)) . D.createVectorDomain $ space

rotOpt :: D.DiscreteSpace -> FuzzyMap -> FuzzyAngle -> FuzzyVector
rotOpt space muA fuzzyAngle v = maximum' . map 
 (\alpha -> min ((fuzzyAngle alpha )) 
               (getMemberShipValue muA (D.fitToVectorDomain (C.rotateClockwise alpha v))) 
 )  $! D.createAngleDomain space

-- |  rotate counter clockwise with a fuzzy vector with a fixed angle
crispRotation :: FuzzyVector -- ^ fuzzy vector
              -> C.Angle     -- ^ fixed angle
              -> FuzzyVector -- ^ rotated fuzzy vector
crispRotation  mu angle v = mu (D.fitToVectorDomain (C.rotateCounterClockwise angle v)) --D.trimtoDiscreteRaster 1 

-- | rotates a fuzzy vector with the centroid of a fuzzy angle
fuzzyCentroidRotation :: D.DiscreteSpace -- ^ space to work on
                      -> FuzzyVector     -- ^ fuzzy vector to rotate
                      -> FuzzyAngle      -- ^ fuzzy angle where centroid is used to rotate the fuzzy vector
                      -> FuzzyVector     -- ^ rotated fuzzy vector
fuzzyCentroidRotation space muA fuzzyAngle v= crispRotation muA centroidAngle v
 where !centroidAngle = calcCentroidAngle space fuzzyAngle

-- | calculates the centroid of a fuzzy angle
calcCentroidAngle :: D.DiscreteSpace -- ^ space to work on
                  -> FuzzyAngle      -- ^ fuzzy angle
                  -> C.Angle         -- ^ centroid angle from the fuzzy angle
calcCentroidAngle space muangle = (/m). sum.map (\l-> (muangle l)*l) $! D.createAngleDomain space 
 where !m =  sum.map muangle $! D.createAngleDomain space   

-- | debug function
debugFuzzyRotation :: D.DiscreteSpace -- ^  
                   -> FuzzyVector     -- ^
                   -> FuzzyAngle      -- ^
                   -> C.CrispVector   -- ^
                   -> [(C.Angle,C.CrispVector,C.CrispVector,MemberShipValue)]
debugFuzzyRotation space muA fuzzyAngle v = map 
 (\alpha -> ( alpha
            , v
            , (D.fitToVectorDomain (C.rotateCounterClockwise alpha v))
            , min ((fuzzyAngle alpha )) 
                  (muA (D.fitToVectorDomain (C.rotateCounterClockwise alpha v)) )
            )) $ D.createAngleDomain space


-- * translate and center crisp

-- | translates a fuzzy vector to the origin 
-- as translation vector the centroid of the input fuzzy vector is used
translateToCenterWithCentroid :: D.DiscreteSpace -- ^ space to work on
                              -> FuzzyMap     -- ^ fuzzy vector to translate 
                              -> FuzzyMap     -- ^ centered fuzzy vector
translateToCenterWithCentroid space muA  = crispTranslate muA centroid 
 where !centroid = D.fitToVectorDomain $! calcCentroid space muA 

-- | translate a fuzzy vector with a fixed vector
crispTranslate :: FuzzyMap   -- ^ fuzzy vector
               -> C.CrispVector -- ^ fixed vector
               -> FuzzyMap   -- ^ translated vector
crispTranslate muA trans = Map.mapKeys (\v -> C.add v trans') muA
 where trans' = C.scaleCrispVector (-1) trans
 -- is add here correct?

-- | calculates the centroid of a fuzzy vector
calcCentroid' :: D.DiscreteSpace -- ^ space to work on
              -> FuzzyVector     -- ^ fuzzy vector
              -> C.CrispVector   -- ^ centroid of the fuzzy vector
calcCentroid' space muA = D.fitToVectorDomain. C.scaleCrispVector (1/m) . C.sumVec . map
 (\v -> C.scaleCrispVector (muA v) v) $! D.createVectorDomain space
 where !m =sum . map (muA) $! D.createVectorDomain space             

calcCentroid :: D.DiscreteSpace
             -> FuzzyMap
             -> C.CrispVector
calcCentroid space mu =   D.fitToVectorDomain. C.scaleCrispVector (1/m) . C.sumVec . map
  (\v -> C.scaleCrispVector (getMemberShipValue mu v) v) $! D.createVectorDomain space
 where !m = Map.fold (+) 0 mu           
