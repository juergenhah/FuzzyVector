{-|
Module      : FuzzyExamples
Description : includes fuzzy vectors used for examples
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

Implementation of fuzzy vectors (fork, spoon, and others) presented in the COSIT paper 2015
-}
module FuzzyExamples where

import CrispVector
import FuzzyTemplates
import FuzzyVector
import BinaryFuzzyOperation
import Discretization

originTrapez :: CrispVector  
             -> MemberShipValue
originTrapez = symetric2DTrapez 0 1 1 1

originTrapezMap space = toFuzzyMap space originTrapez

fiveFiveTrapez :: CrispVector 
               -> MemberShipValue
fiveFiveTrapez = symetric2DTrapez 5 2 1 1

fiveFiveTrapezMap space = toFuzzyMap space fiveFiveTrapez

fuzzyFork :: CrispVector 
          -> MemberShipValue
fuzzyFork  v = min (xTrapez $ getX v) (yTrapez $ getY v)
 where xTrapez = trapez (-6) 1 1 1
       yTrapez = trapez 0 6 1 1

fuzzyForkMap space = toFuzzyMap space fuzzyFork

fuzzySpoon :: CrispVector 
           -> MemberShipValue
fuzzySpoon v = min (xTrapez $ getX v) (yTrapez $ getY v)
 where xTrapez = trapez 4 4 1 1
       yTrapez = trapez 0 2 2 2

fuzzySpoonMap space = toFuzzyMap space fuzzySpoon

fuzzyRealSpoon :: CrispVector 
               -> MemberShipValue
fuzzyRealSpoon = fuzzyUnion g handle 
 where        g = fuzzyCircleTemplate 3 2 (crispVector (-5.0) 0.0 )     

fuzzyRealSpoonMap space = toFuzzyMap space fuzzyRealSpoon

handle v= min (xTrapez $ getX v) (yTrapez $ getY v)
        where xTrapez = trapez 2 3 2 2
              yTrapez = trapez 0 1 2 2

fuzzySpoon2 :: CrispVector 
            -> MemberShipValue
fuzzySpoon2 v = min (xTrapez $ getX v) (yTrapez $ getY v)
 where xTrapez = trapez 5 2 1 1
       yTrapez = trapez 0 2 2 2

fuzzSpoon2Map space = toFuzzyMap space fuzzySpoon2

house4quadrantMap space = toFuzzyMap space house4quadrant

house :: CrispVector 
      -> MemberShipValue
house v
 | x < 3 || x> 5 =0
 | y < (2) || y> (4)  =0
 | otherwise = 1
 where x = getX v
       y = getY v

houseMap space = toFuzzyMap space house

houseFront ::CrispVector 
           -> MemberShipValue
houseFront v
 |  x== 5 && y==3  =1
 | otherwise = 0
 where x = getX v
       y = getY v       

houseFrontMap space = toFuzzyMap space houseFront

person :: CrispVector 
       -> MemberShipValue
person v
 | x < 3 || x> 4  =0
 | y < (7)  || y> (8) =0
 | otherwise = 1
 where x = getX v
       y = getY v

personMap space = toFuzzyMap space person

-- | needed for figure person sub house
house4quadrant :: CrispVector 
               -> MemberShipValue
house4quadrant v
 | x < 0 || x> 10 =0 --2 4
 | y > (-5) || y< (-15)  =0
 | otherwise = 1
 where x = getX v
       y = getY v

-- | needed for figure person sub house
person4quadrant :: CrispVector 
                -> MemberShipValue
person4quadrant v
 | x < 2 || x> 4  =0
 | y > (-2)  || y< (-4) =0
 | otherwise = 1
 where x = getX v
       y = getY v

person4quadrantMap space= toFuzzyMap space person4quadrant

objA :: CrispVector
     -> MemberShipValue
objA v     
 | x < 0 || x> 1  =0
 | y < (7)  || y> (8) =0
 | otherwise = 1
 where x = getX v
       y = getY v

objAMap space= toFuzzyMap space objA

centerobjA :: CrispVector
           -> MemberShipValue
centerobjA v     
 | x < (-1)  || x> 1  =0
 | y < (-1)  || y> (1) =0
 | otherwise = 1
 where x = getX v
       y = getY v

centerobjAMap space= toFuzzyMap space centerobjA

objB :: CrispVector
     -> MemberShipValue
objB v =min (xTrapez $ getX v) (yTrapez $ getY v)
 where xTrapez = trapez (6) 1 2 2
       yTrapez = trapez 1 1 2 2

objBMap space= toFuzzyMap space objB

-- * objects for the reference frame transformations

largehouse  :: CrispVector 
            -> MemberShipValue
largehouse v
 | x < (-12) || x> (-2) =0  
 | y < (6) || y> (15)  =0   
 | otherwise = 1
   where x = getX v
         y = getY v

largehouseMap space= toFuzzyMap space largehouse

largehouseFront :: CrispVector
                 -> MemberShipValue
largehouseFront v 
 | x < (-4) || x> (-3)   =0
 | y < (-8)  || y> (-7) =0
 | otherwise = 1
 where x = getX v
       y = getY v

largehouseFrontMap space= toFuzzyMap space largehouseFront

realLargehouseFront  :: CrispVector
                     -> MemberShipValue
realLargehouseFront v 
 | x < (-9) || x> (-5)  =0  
 | y < (5) || y> (6) = 0    
 | otherwise = 1
  where x = getX v
        y = getY v
        
realLargehouseFrontMap space= toFuzzyMap space realLargehouseFront

tree :: CrispVector
     -> MemberShipValue
tree = fuzzyCircleTemplate 1 3 (crispVector 6.0 7.0)     

treeMap space= toFuzzyMap space tree

observer :: CrispVector 
         -> MemberShipValue
observer v
 | x < 0 || x> 1  =0
 | y < (0)  || y> (1) =0
 | otherwise = 1
 where x = getX v
       y = getY v

observerMap space= toFuzzyMap space observer

north :: CrispVector
      -> MemberShipValue
north v
 | x < (-9) || x> (-8)   =0
 | y < (7)  || y> (8) =0
 | otherwise = 1
 where x = getX v
       y = getY v
northMap space= toFuzzyMap space north
