{-|
Module      : FuzzyTemplates
Description : functions to genereate fuzzy templates like, circles, directions, trapez
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

-}
module FuzzyTemplates ( trapez
                      , symetric2DTrapez
                      , fuzzyCircleTemplate
                      , fuzzyDirectionTemplate
                      ) where

import           CrispVector
import           FuzzyVector

-- * One dimensional fuzzy templates

-- | One dimensional trapez fuzzy value
-- definition taken from page 16 from the book of Prof. Viertel
trapez :: Double          -- ^ m
       -> Double          -- ^ s 
       -> Double          -- ^ l 
       -> Double          -- ^ r
       -> Double          -- ^ x
       -> MemberShipValue -- ^ one dimensional fuzzy value
trapez m s l r x = 
 if m-s  <= x && x <= m+s 
    then 1
    else if x>m+s && x<= m+s+r
            then (m+s+r-x)/r
            else if x<m-s && x>=m-s-l
                    then (x-m+s+l)/l 
                    else 0

-- * Two dimensional fuzzy templates

-- | symetric two dimensional trapez
symetric2DTrapez :: Double          -- ^ m 
                 -> Double          -- ^ s 
                 -> Double          -- ^ l
                 -> Double          -- ^ r 
                 -> CrispVector     -- ^ vector
                 -> MemberShipValue -- ^ membership value for vector v
symetric2DTrapez m s l r v = min (xTrapez $ getX v) (yTrapez $ getY v)
 where xTrapez = trapez m s l r
       yTrapez = trapez m s l r       

-- | circle template with borders
fuzzyCircleTemplate :: Double          -- ^ radius of center to have membership value 1
                    -> Double          -- ^ down slope of length l
                    -> CrispVector     -- ^ center of the circle
                    -> CrispVector     -- ^ actual vector 
                    -> MemberShipValue -- ^ membership value for actual vector
fuzzyCircleTemplate radius l center v  
 | isInsideCircle radius center v = 1
 | isInsideCircle (radius+l) center v = (radius-(distance center v) +l)/l 
 | otherwise = 0

-- | checks if a vector is inside a circle
isInsideCircle :: Double      -- ^ circle of the radius
               -> CrispVector -- ^ center of the circle
               -> CrispVector -- ^ actual vector to ask
               -> Bool        -- ^ is actual vector inside circle?
isInsideCircle radius center v
 | (distance center v <= radius) = True
 | (distance center v > radius) = False


-- TODO
-- | template for a direction, between start and end angle
fuzzyDirectionTemplate :: Angle           -- ^ start angle
                       -> Angle           -- ^ end angle
                       -> Double          -- ^ border
                       -> CrispVector     -- ^ actual vector
                       -> MemberShipValue -- ^ membership value for the actual vector
fuzzyDirectionTemplate  left right s v  
 | isBetweenAngles left right alpha = 1.0
 | isBetweenAngles (normalize (right)) (normalize (right) +s) alpha = 0.2
 | isBetweenAngles (normalize (left) - s) (normalize left) alpha = 0.2
 | otherwise = 0.0
 where alpha = calcAngleFromYAxisClockwise v

-- | checks if an angle is between two angles
isBetweenAngles :: Angle -- ^ left angle 
                -> Angle -- ^ right angle
                -> Angle -- ^ angle to test if between left and right
                -> Bool  -- ^ is angle between left and right?
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

-- | fits an arbitrary angle to the angle domain [0,..360]
normalize :: Angle -- ^ angle to fit to angle domain
          -> Angle -- ^ angle between [0,..360]
normalize alpha
 | 0.0 > degrees = degrees + 360.0
 | otherwise = degrees
 where circleCount = floor  (alpha / 360.0) 
       degrees = alpha - fromIntegral (circleCount) *360.0
