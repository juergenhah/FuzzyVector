{-# LANGUAGE BangPatterns #-}
{-|
Module      : CrispVector
Description : abstraction from Data.Vector package to create a crisp vector
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 
-}
module CrispVector ( CrispVector (..)
                   , Angle
                   , crispVector
                   , getX
                   , getY
                   , add
                   , sub
                   , scaleCrispVector
                   , sumVec
                   , distance
                   , calcAngleFromYAxisClockwise
                   , rotateCounterClockwise
                   , rotateClockwise
                   ) where

import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N
import Control.Parallel.Strategies
import Control.DeepSeq
import Data.List.Extra

-- | Abstraction from the data.vector package
data CrispVector = C (V.Vector Double) deriving (Eq,Ord,Read,Show)

instance NFData CrispVector where
 rnf (C v) = rnf v
 
-- | represents an Angle of an vector
type Angle = Double 

-- | creates an crisp vector from x and y coordinate
crispVector :: Double      -- ^ x coordinate of crisp vector
            -> Double      -- ^ y coordinate of crisp vector
            -> CrispVector -- ^ crisp vector build from x and y
crispVector x y = C (V.fromList [x,y]) 

-- | extract X value of the vector
getX :: CrispVector -- ^ crisp vector with at least 1 dimension
     -> Double      -- ^ first element of the crisp vector
getX (C v) = V.head v    

-- | extract Y value of the vector
getY :: CrispVector -- ^ crisp vector with at least 2 dimensions
     -> Double      -- ^ second element of the crisp vector
getY (C v) = V.head . V.drop 1 $ v   

-- | multiplies each element of the vector
scaleCrispVector :: Double      -- ^ multiplier for vector
                 -> CrispVector -- ^ vector to be scaled
                 -> CrispVector -- ^ multiplied vector
scaleCrispVector lambda (C v)  =C (V.map (*lambda) v )

-- | creates the sum of the vectors included into the list
-- for an empty list a 0.0 0.0 vector is returned
sumVec :: [CrispVector] -- ^ list of crisp vectors
       -> CrispVector   -- ^ sum of all vectors from the list
sumVec [] = crispVector 0.0 0.0 -- error case
sumVec (v:vs) =add v $ sumVec vs 

-- | adds two vectors
add :: CrispVector
    -> CrispVector
    -> CrispVector
add (C a ) (C b)  = C (a+b)

-- | subtracts the second vector from the first
sub :: CrispVector
     -> CrispVector
     -> CrispVector
sub (C a ) (C b)  = C (a-b)

-- | calculates the euclidian distance of two points indicated by two crisp vectors
distance :: CrispVector -- ^ first point - vector
         -> CrispVector -- ^ second point - vector 
         -> Double      -- ^ distance
distance center w = sqrt((x1-x2)**2 + (y1-y2)**2)
 where x1 = getX center
       y1 = getY center
       x2 = getX w
       y2 = getY w       

-- | calculates the angle in degree of an vector in respect of the y axis
-- y yxis == North direction 
calcAngleFromYAxisClockwise :: CrispVector -- ^ vector to measure angle
                            -> Angle       -- ^ angle from vector in degree
calcAngleFromYAxisClockwise v
 | x<0=360+ (todegree$! atan2 x y) 
 | otherwise = todegree $!atan2 x y
 where !x = getX v
       !y = getY v
       todegree b = b*180/pi 

-- | rotates a crisp vector with an angle counter clockwise
rotateCounterClockwise :: Angle       -- ^ angle in degree
                       -> CrispVector -- ^ vector to rotate
                       -> CrispVector -- ^ rotated vector
rotateCounterClockwise alpha (!C v)  = C (ccRotmatrix N.<> v) 
  where !ccRotmatrix = (2N.><2) [ cos radAlpha, -sin radAlpha 
                                , sin radAlpha, cos radAlpha]
        !radAlpha = alpha*pi/180

-- | rotates a crisp vector with an angle clockwise
rotateClockwise :: Angle       -- ^ angle in degree
                -> CrispVector -- ^ vector to rotate
                -> CrispVector -- ^ rotated vector
rotateClockwise !alpha (!C v)  = C (cRotmatrix N.<> v )
 where !cRotmatrix = (2N.><2) [ cos radAlpha, sin radAlpha 
                              , -sin radAlpha,cos radAlpha]
       !radAlpha = alpha *pi/180
