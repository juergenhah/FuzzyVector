module ReferenceFrames where
import qualified Data.Vector.Storable as V

import Discretization
import Distributions
import BinaryFuzzyOperation
import GHC.Float

-- * ground template

fuzzyGroundTemplate :: CrispVector
                    -> Probability
fuzzyGroundTemplate = fuzzyCircleTemplate 3 0 (V.fromList [0.0,0.0])

bigfuzzyGroundTemplate :: CrispVector
                       -> Probability
bigfuzzyGroundTemplate = fuzzyCircleTemplate 5 0 (V.fromList [0.0,0.0])
-- * reference frames
allRefFrames = [fuzzyHere,fuzzyNear,fuzzyFar,fuzzyVeryFar,fuzzyFront,fuzzyBack,fuzzyRight,fuzzyLeft,
                fuzzyFrontRight,fuzzyFrontLeft,fuzzyBackRight,fuzzyBackLeft]
allRefFramesNames = ["here","near","far","veryfar","front","back","right","left"
                    ,"front-right","front-left","back-right","back-left"]                
namedRefFrames = zipWith (\a b -> (a,b)) allRefFramesNames allRefFrames
                   
-- hear following the definition of Frank
fuzzyHere :: CrispVector 
            -> Probability
fuzzyHere = fuzzyCircleTemplate 1 1 (V.fromList [0.0,0.0])

-- near
fuzzyNear :: CrispVector 
          -> Probability
fuzzyNear = fuzzyComplement (fuzzyCircleTemplate 2 1 (V.fromList [0.0,0.0]))  fuzzyHere

-- far
fuzzyFar :: CrispVector 
         -> Probability
fuzzyFar = fuzzyComplement (fuzzyCircleTemplate 4 1 (V.fromList [0.0,0.0]))  (fuzzyUnion fuzzyNear fuzzyHere) 

fuzzyVeryFar :: CrispVector
             -> Probability
fuzzyVeryFar = fuzzyComplement (fuzzyCircleTemplate 6 2 (V.fromList [0.0,0.0]))  (fuzzyUnion fuzzyFar $ fuzzyUnion fuzzyNear fuzzyHere)       


-- * directions --TODO how it is called in the literature
-- get angles from eye studies?
fuzzyFront :: CrispVector 
           -> Probability
fuzzyFront = fuzzyDirectionTemplate (300) (60) 5 

fuzzyBack :: CrispVector
          -> Probability
fuzzyBack = fuzzyDirectionTemplate (120) (240) 5  

fuzzyRight :: CrispVector
           -> Probability
fuzzyRight = fuzzyDirectionTemplate (30) (150) 5  

fuzzyLeft :: CrispVector
          -> Probability
fuzzyLeft = fuzzyDirectionTemplate (210) (330) 5  

fuzzyFrontRight :: CrispVector
                -> Probability
fuzzyFrontRight = fuzzyIntersection  fuzzyFront fuzzyRight

fuzzyFrontLeft :: CrispVector
               -> Probability
fuzzyFrontLeft = fuzzyIntersection fuzzyFront fuzzyLeft

fuzzyBackRight :: CrispVector
               -> Probability
fuzzyBackRight = fuzzyIntersection fuzzyBack fuzzyRight                              

fuzzyBackLeft :: CrispVector
                   -> Probability
fuzzyBackLeft = fuzzyIntersection fuzzyBack fuzzyLeft

fuzzyDirectionTemplate :: Angle -> Angle -> Double-> CrispVector -> Probability
fuzzyDirectionTemplate  left right s v  
 | isBetweenAngles left right alpha = 1.0
 | isBetweenAngles (normalize (right)) (normalize (right) +s) alpha = 0.2
 | isBetweenAngles (normalize (left) - s) (normalize left) alpha = 0.2 --normalize ( normalize (normalize (left-s)- alpha)+s) /s
 | otherwise = 0.0
 where alpha = calcAngleFromYAxisClockwise v


isBetweenAngles :: Angle -> Angle -> Angle ->Bool 
isBetweenAngles left right alpha 
 | right - left >=360 = True
 | (degrees < startDegrees) || (degrees > endDegrees)=False
 | otherwise = True
 where degrees = normalize alpha
       startDegrees  = if normalize (left) > degrees
                          then normalize (left) -360
                          else normalize left
       endDegrees = if normalize (right) < startDegrees
                       then normalize (right) +360
                       else if (normalize (right) - startDegrees)>=360
                            then normalize (right) -360
                            else normalize (right)

 

normalize ::Angle  ->Angle
normalize alpha
 | 0.0 > degrees = degrees + 360.0
 | otherwise = degrees
 where circleCount = floor  (alpha / 360.0) 
       degrees = alpha - fromIntegral (circleCount) *360.0



-- farright -- intersection of far and right
