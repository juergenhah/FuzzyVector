{-|
Module      : Distributions
Description : Implements fuzzy distributions
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

Implementation of fuzzy distributions (fork and spoon)  presented in the COSIT paper 2015
-}
 module Distributions where
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra  as N


type Probability = Double
type Distribution= [Probability]
type CrispVector = V.Vector Double

-- * One dimensional functions

-- | One dimensional fuzzy Function
-- definition taken from page 16 Viertel book
trapez :: Double 
       -> Double
       -> Double
       -> Double 
       -> Double 
       -> Probability
trapez m s l r x = if m-s  <= x && x <= m+s 
                     then 1
                     else if x>m+s && x<= m+s+r
                             then (m+s+r-x)/r
                             else if x<m-s && x>=m-s-l
                                     then (x-m+s+l)/l 
                                     else 0

-- * Two dimensional functions

symetric2DTrapez :: Double 
                 -> Double
                 -> Double
                 -> Double 
                 -> CrispVector 
                 -> Probability
symetric2DTrapez m s l r v = min (xTrapez x) (yTrapez y)
 where xTrapez = trapez m s l r
       yTrapez = trapez m s l r
       x = V.head v
       y = V.head.V.drop 1 $ v       

originTrapez :: CrispVector  
             -> Probability
originTrapez = symetric2DTrapez 0 1 1 1

fiveFiveTrapez :: CrispVector 
               -> Probability
fiveFiveTrapez = symetric2DTrapez 5 2 1 1

fuzzyFork :: CrispVector 
          -> Probability
fuzzyFork  v = min (xTrapez x) (yTrapez y)
  where xTrapez = trapez (-6) 1 1 1
        yTrapez = trapez 0 6 1 1
        x = V.head v
        y = V.head.V.drop 1 $ v       

fuzzySpoon :: CrispVector 
           -> Probability
fuzzySpoon v = min (xTrapez x) (yTrapez y)
  where xTrapez = trapez 4 4 1 1
        yTrapez = trapez 0 2 2 2
        x = V.head v
        y = V.head.V.drop 1 $ v       

fuzzySpoon2 :: CrispVector 
            -> Probability
fuzzySpoon2 v = min (xTrapez x) (yTrapez y)
  where xTrapez = trapez 5 2 1 1
        yTrapez = trapez 0 2 2 2
        x = V.head v
        y = V.head.V.drop 1 $ v       

subFork :: CrispVector 
        -> Probability
subFork v
 | x < 0 || x> 1 =0
 | y < 2 || y> 3 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v


house4quadrant :: CrispVector 
               -> Probability
house4quadrant v
 | x < 2 || x> 4 =0
 | y > (-6) || y< (-8)  =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

house :: CrispVector 
               -> Probability
house v
 | x < 3 || x> 5 =0
 | y < (2) || y> (4)  =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

houseFront ::CrispVector 
               -> Probability
houseFront v
            |  x== 5 && y==3  =1
            | otherwise = 0
            where x = V.head v
                  y = V.head.V.drop 1 $ v       
person       :: CrispVector 
              -> Probability
person v
  | x < 3 || x> 4  =0
  | y < (7)  || y> (8) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v

person4quadrant       :: CrispVector 
              -> Probability
person4quadrant v
  | x < 3 || x> 4  =0
  | y > (-5)  || y< (-6) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v

objA :: CrispVector
     -> Probability
objA v     
  | x < 0 || x> 1  =0
  | y < (7)  || y> (8) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v

centerobjA     :: CrispVector
     -> Probability
centerobjA v     
  | x < (-1)  || x> 1  =0
  | y < (-1)  || y> (1) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v
        
objB  :: CrispVector
      -> Probability
objB v =min (xTrapez x) (yTrapez y)
  where xTrapez = trapez (6) 1 2 2
        yTrapez = trapez 1 1 2 2
        x = V.head v
        y = V.head.V.drop 1 $ v       
 {-    | x < 3 || x> 4  =0
      | y < (1)  || y> (2) =0
      | otherwise = 1
      where x = V.head v
            y = V.head.V.drop 1 $ v
-}

newFork :: CrispVector 
        -> Probability
newFork v
 | x < 0 || x> 1 =0
 | y < 3 || y> 4 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v


myFork :: CrispVector 
       -> Probability
myFork v
 | x < 0 || x> 1 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

toRotFork :: CrispVector 
          -> Probability
toRotFork v
 | x < 0 || x> 1 =0
 | y < 0 || y> 2 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v
       

toRotSpoon :: CrispVector 
           -> Probability
toRotSpoon v
 | x < 0 || x> 2 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v
       
normSpoon :: CrispVector
          -> Probability
normSpoon v
 | x < 4 || x> 5 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

mySpoon :: CrispVector
        -> Probability
mySpoon v
 | x < 2 || x> 3 =0
 | y < 2 || y> 3 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

myPyramide :: CrispVector
           -> Probability
myPyramide v
 | x<0 || y<0 = 0
 | x>=5 || y>=5 = 0
 | (0<= x && x<= 1) && (0<=y && y<= 1)=1
 | otherwise = (-1/4) * sqrt(x*x+y*y) +6/4
   where x = V.head v
         y = V.head.V.drop 1 $ v

mySteps :: CrispVector
        -> Probability
mySteps v
 | (x >= (2+xdel) && x<=(3+xdel)) && (y >= (2+ydel) && y<=(3+ydel))=1
 | (x >= (1+xdel) && x<=(4+xdel)) && (y >= (1+ydel) && y<=(4+ydel))=0.66
 | (x >= (0+xdel) && x<=(5+xdel)) && (y >= (0+ydel) && y<=(5+ydel))=0.33
 | otherwise = 0
    where x = V.head v
          y = V.head.V.drop 1 $ v
          xdel= 2
          ydel= 1

myXAxis :: CrispVector
        -> Probability
myXAxis v
 | x  <3 || x>4 =0
 | y  <0 || y>1 =0
 |  otherwise =1
 where x = V.head v
       y = V.head.V.drop 1 $ v

myYAxis :: CrispVector 
        -> Probability
myYAxis v
 | x  <0 || x>1 =0
 | y  <7 || y>8 =0
 | otherwise =1
 where x = V.head v
       y = V.head.V.drop 1 $ v


-- * objects for the reference frame transforamtions

largehouse  :: CrispVector 
            -> Probability
largehouse v
 | x < 2 || x> 10 =0
 | y < (12) || y> (17)  =0
 | otherwise = 1
   where x = V.head v
         y = V.head.V.drop 1 $ v

largehouseFront :: CrispVector
                 -> Probability
largehouseFront v 
 | (x>3 && 9>x )   && (y>10 && 12>y)   =1                 
 | otherwise = 0
   where x = V.head v
         y = V.head.V.drop 1 $ v


tree :: CrispVector
     -> Probability
tree = fuzzyCircleTemplate 1 1 (V.fromList [15.0,10.0])     

simon       :: CrispVector 
                     -> Probability
simon v
  | x < 0 || x> 1  =0
  | y < (0)  || y> (1) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v
        
north :: CrispVector
      -> Probability
north v
   | x < 0 || x> 1  =0
   | y < (5)  || y> (6) =0
   | otherwise = 1
   where x = V.head v
         y = V.head.V.drop 1 $ v

fuzzyCircleTemplate :: Double-> Double -> CrispVector -> CrispVector -> Probability
fuzzyCircleTemplate radius l center v  
 | isInsideCircle radius center v = 1
 | isInsideCircle (radius+l) center v = (radius-(distance center v) +l)/l 
 | otherwise = 0

distance :: CrispVector -> CrispVector -> Double
distance center w = sqrt((x1-x2)**2 + (y1-y2)**2)
 where x1 =V.head center
       y1 = V.head.V.drop 1  $ center
       x2 =V.head w
       y2 =V.head.V.drop 1  $ w       

isInsideCircle :: Double-> CrispVector -> CrispVector -> Bool
isInsideCircle radius center v
 | (distance center v <= radius) = True
 | (distance center v > radius) = False
