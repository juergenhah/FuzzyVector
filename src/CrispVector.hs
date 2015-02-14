{-|
Module      : CrispVector
Description : abstraction from Data.Vector package and functions for crisp vector
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 
-}
module CrispVector ( CrispVector
                   , Angle
                   , crispVector
                   , getX
                   , getY
                   , scaleCrispVector
                   , sumVec
                   , distance
                   , calcAngleFromYAxisClockwise
                   , rotateCounterClockwise
                   , rotateClockwise
                   ) where

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N

-- | Abstraction from the data.vector package
type CrispVector = V.Vector Double

-- | represents an Angle of an vector
type Angle = Double 

-- | creates an crisp vector from x and y coordinate
crispVector :: Double      -- ^ x coordinate of crisp vector
            -> Double      -- ^ y coordinate of crisp vector
            -> CrispVector -- ^ crisp vector build from x and y
crispVector x y = V.fromList [x,y]

-- | extract X value of the vector
getX :: CrispVector -- ^ crisp vector with at least 1 dimension
     -> Double      -- ^ first element of the crisp vector
getX = V.head     

-- | extract Y value of the vector
getY :: CrispVector -- ^ crisp vector with at least 2 dimensions
     -> Double      -- ^ second element of the crisp vector
getY = V.head . V.drop 1    

-- | multiplies each element of the vector
scaleCrispVector :: Double      -- ^ multiplier for vector
                 -> CrispVector -- ^ vector to be scaled
                 -> CrispVector -- ^ multiplied vector
scaleCrispVector lambda v =V.map (*lambda) v 

-- | creates the sum of the vectors included into the list
-- for an empty list a 0.0 0.0 vector is returned
sumVec :: [CrispVector] -- ^ list of crisp vectors
       -> CrispVector   -- ^ sum of all vectors from the list
sumVec [] = crispVector 0.0 0.0 -- error case
sumVec (v:vs) = v + sumVec vs

-- | calculates the euclidian distance of two points indicated by two crisp vectors
distance :: CrispVector -- ^ first point - vector
         -> CrispVector -- ^ second point - vector 
         -> Double      -- ^ distance
distance center w = sqrt((x1-x2)**2 + (y1-y2)**2)
 where x1 =V.head center
       y1 = V.head.V.drop 1  $ center
       x2 =V.head w
       y2 =V.head.V.drop 1  $ w       

-- | calculates the angle in degree of an vector in respect of the y axis
-- y yxis == North direction 
calcAngleFromYAxisClockwise :: CrispVector -- ^ vector to measure angle
                            -> Angle       -- ^ angle from vector in degree
calcAngleFromYAxisClockwise v
 | x<0=360+ (todegree$ atan2 x y) 
 | otherwise = todegree $atan2 x y
 where x = V.head v
       y = V.head.V.drop 1 $ v
       todegree b = b*180/pi 

-- | rotates a crisp vector with an angle counter clockwise
rotateCounterClockwise :: Angle       -- ^ angle in degree
                       -> CrispVector -- ^ vector to rotate
                       -> CrispVector -- ^ rotated vector
rotateCounterClockwise alpha v = ccRotmatrix N.<> v
  where ccRotmatrix = (2N.><2) [ cos radAlpha, -sin radAlpha 
                               , sin radAlpha, cos radAlpha]
        radAlpha = alpha*pi/180

-- | rotates a crisp vector with an angle clockwise
rotateClockwise :: Angle       -- ^ angle in degree
                -> CrispVector -- ^ vector to rotate
                -> CrispVector -- ^ rotated vector
rotateClockwise alpha v =  cRotmatrix N.<> v 
 where cRotmatrix = (2N.><2) [ cos radAlpha, sin radAlpha 
                             , -sin radAlpha,cos radAlpha]
       radAlpha = alpha *pi/180
