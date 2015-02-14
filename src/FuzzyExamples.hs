{-|
Module      : FuzzyExamples
Description : defines fuzzy vectors
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

Implementation of fuzzy vectors (fork, spoon, and others) presented in the COSIT paper 2015
-}
 module FuzzyExamples where
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N

import CrispVector
import FuzzyTemplates
import FuzzyVector

originTrapez :: CrispVector  
             -> MemberShipValue
originTrapez = symetric2DTrapez 0 1 1 1

fiveFiveTrapez :: CrispVector 
               -> MemberShipValue
fiveFiveTrapez = symetric2DTrapez 5 2 1 1

fuzzyFork :: CrispVector 
          -> MemberShipValue
fuzzyFork  v = min (xTrapez x) (yTrapez y)
  where xTrapez = trapez (-6) 1 1 1
        yTrapez = trapez 0 6 1 1
        x = V.head v
        y = V.head.V.drop 1 $ v       

fuzzySpoon :: CrispVector 
           -> MemberShipValue
fuzzySpoon v = min (xTrapez x) (yTrapez y)
  where xTrapez = trapez 4 4 1 1
        yTrapez = trapez 0 2 2 2
        x = V.head v
        y = V.head.V.drop 1 $ v       

fuzzySpoon2 :: CrispVector 
            -> MemberShipValue
fuzzySpoon2 v = min (xTrapez x) (yTrapez y)
  where xTrapez = trapez 5 2 1 1
        yTrapez = trapez 0 2 2 2
        x = V.head v
        y = V.head.V.drop 1 $ v       

house4quadrant :: CrispVector 
               -> MemberShipValue
house4quadrant v
 | x < 2 || x> 4 =0
 | y > (-6) || y< (-8)  =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

house :: CrispVector 
               -> MemberShipValue
house v
 | x < 3 || x> 5 =0
 | y < (2) || y> (4)  =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

houseFront ::CrispVector 
               -> MemberShipValue
houseFront v
            |  x== 5 && y==3  =1
            | otherwise = 0
            where x = V.head v
                  y = V.head.V.drop 1 $ v       
person       :: CrispVector 
              -> MemberShipValue
person v
  | x < 3 || x> 4  =0
  | y < (7)  || y> (8) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v

person4quadrant       :: CrispVector 
              -> MemberShipValue
person4quadrant v
  | x < 3 || x> 4  =0
  | y > (-5)  || y< (-6) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v

objA :: CrispVector
     -> MemberShipValue
objA v     
  | x < 0 || x> 1  =0
  | y < (7)  || y> (8) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v

centerobjA :: CrispVector
           -> MemberShipValue
centerobjA v     
  | x < (-1)  || x> 1  =0
  | y < (-1)  || y> (1) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v
        
objB :: CrispVector
     -> MemberShipValue
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

-- * objects for the reference frame transforamtions

largehouse  :: CrispVector 
            -> MemberShipValue
largehouse v
 | x < 2 || x> 10 =0
 | y < (12) || y> (17)  =0
 | otherwise = 1
   where x = V.head v
         y = V.head.V.drop 1 $ v

largehouseFront :: CrispVector
                 -> MemberShipValue
largehouseFront v 
 | (x>3 && 9>x )   && (y>10 && 12>y)   =1                 
 | otherwise = 0
   where x = V.head v
         y = V.head.V.drop 1 $ v


tree :: CrispVector
     -> MemberShipValue
tree = fuzzyCircleTemplate 1 1 (V.fromList [15.0,10.0])     

simon       :: CrispVector 
                     -> MemberShipValue
simon v
  | x < 0 || x> 1  =0
  | y < (0)  || y> (1) =0
  | otherwise = 1
  where x = V.head v
        y = V.head.V.drop 1 $ v
        
north :: CrispVector
      -> MemberShipValue
north v
   | x < 0 || x> 1  =0
   | y < (5)  || y> (6) =0
   | otherwise = 1
   where x = V.head v
         y = V.head.V.drop 1 $ v


 {-
  subFork :: CrispVector 
          -> MemberShipValue
  subFork v
   | x < 0 || x> 1 =0
   | y < 2 || y> 3 =0
   | otherwise = 1
   where x = V.head v
         y = V.head.V.drop 1 $ v
 
newFork :: CrispVector 
        -> MemberShipValue
newFork v
 | x < 0 || x> 1 =0
 | y < 3 || y> 4 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v


myFork :: CrispVector 
       -> MemberShipValue
myFork v
 | x < 0 || x> 1 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

toRotFork :: CrispVector 
          -> MemberShipValue
toRotFork v
 | x < 0 || x> 1 =0
 | y < 0 || y> 2 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v
       

toRotSpoon :: CrispVector 
           -> MemberShipValue
toRotSpoon v
 | x < 0 || x> 2 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v
       
normSpoon :: CrispVector
          -> MemberShipValue
normSpoon v
 | x < 4 || x> 5 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

mySpoon :: CrispVector
        -> MemberShipValue
mySpoon v
 | x < 2 || x> 3 =0
 | y < 2 || y> 3 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

myPyramide :: CrispVector
           -> MemberShipValue
myPyramide v
 | x<0 || y<0 = 0
 | x>=5 || y>=5 = 0
 | (0<= x && x<= 1) && (0<=y && y<= 1)=1
 | otherwise = (-1/4) * sqrt(x*x+y*y) +6/4
   where x = V.head v
         y = V.head.V.drop 1 $ v

mySteps :: CrispVector
        -> MemberShipValue
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
        -> MemberShipValue
myXAxis v
 | x  <3 || x>4 =0
 | y  <0 || y>1 =0
 |  otherwise =1
 where x = V.head v
       y = V.head.V.drop 1 $ v

myYAxis :: CrispVector 
        -> MemberShipValue
myYAxis v
 | x  <0 || x>1 =0
 | y  <7 || y>8 =0
 | otherwise =1
 where x = V.head v
       y = V.head.V.drop 1 $ v

-}
