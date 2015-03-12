{-|
Module      : SpatialTemplates
Description : fuzzy vectors representing spatial reference frames
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

-}
module SpatialTemplates where

import BinaryFuzzyOperation
import CrispVector
import FuzzyTemplates
import FuzzyVector
import Discretization 
-- * fuzzy ground templates

fuzzyGroundTemplate :: CrispVector
                    -> MemberShipValue
fuzzyGroundTemplate = fuzzyCircleTemplate 3 0 (crispVector 0.0 0.0)

fuzzyGroundTemplateMap space = toFuzzyMap space fuzzyGroundTemplate

bigfuzzyGroundTemplate :: CrispVector
                       -> MemberShipValue
bigfuzzyGroundTemplate = fuzzyCircleTemplate 4 3 (crispVector 0.0 0.0)

bigfuzzyGroundTemplateMap space = toFuzzyMap space bigfuzzyGroundTemplate

-- * fuzzy spatial templates
allSpatialTemplates space = [fuzzyHereMap space ,fuzzyNearMap space ,fuzzyFarMap space ,fuzzyVeryFarMap space ,fuzzyFrontMap space ,fuzzyBackMap space,fuzzyRightMap space ,fuzzyLeftMap space,
                                      fuzzyFrontRightMap space,fuzzyFrontLeftMap space ,fuzzyBackRightMap space ,fuzzyBackLeftMap space]

allSpatialTemplateNames = ["here","near","far","veryfar","front","back","right","left"
                                         ,"front-right","front-left","back-right","back-left"]                

namedSpatialTemplates space= zipWith (\a b -> (a,b)) allSpatialTemplateNames (allSpatialTemplates space)

fuzzyHere :: CrispVector 
            -> MemberShipValue
fuzzyHere = fuzzyCircleTemplate 4 0 (crispVector 0.0 0.0)

fuzzyHereMap space = toFuzzyMap space fuzzyHere
-- near
fuzzyNear :: CrispVector 
          -> MemberShipValue
fuzzyNear = fuzzyComplement' (fuzzyCircleTemplate 8 0 (crispVector 0.0 0.0))  fuzzyHere

fuzzyNearMap space = toFuzzyMap space fuzzyNear 

-- far
-- needed for the spoonAddFrontLeftFar example
fuzzyFar :: CrispVector 
         -> MemberShipValue
fuzzyFar = fuzzyComplement' (fuzzyCircleTemplate 12 0 (crispVector 0.0 0.0))  (fuzzyUnion' fuzzyNear fuzzyHere) 

fuzzyFarMap space = toFuzzyMap space fuzzyFar 

fuzzyVeryFar :: CrispVector
             -> MemberShipValue
fuzzyVeryFar = fuzzyComplement' (fuzzyCircleTemplate 16 3 (crispVector 0.0 0.0))  (fuzzyUnion' fuzzyFar $ fuzzyUnion' fuzzyNear fuzzyHere)       

fuzzyVeryFarMap space = toFuzzyMap space fuzzyVeryFar

fuzzyFront :: CrispVector 
           -> MemberShipValue
fuzzyFront = fuzzyDirectionTemplate (300) (60) 5 

fuzzyFrontMap space = toFuzzyMap space fuzzyFront

fuzzyBack :: CrispVector
          -> MemberShipValue
fuzzyBack = fuzzyDirectionTemplate (120) (240) 5  

fuzzyBackMap space = toFuzzyMap space fuzzyBack

fuzzyRight :: CrispVector
           -> MemberShipValue
fuzzyRight = fuzzyDirectionTemplate (30) (150) 5  

fuzzyRightMap space = toFuzzyMap space fuzzyRight

fuzzyLeft :: CrispVector
          -> MemberShipValue
fuzzyLeft = fuzzyDirectionTemplate (210) (330) 5 

fuzzyLeftMap space = toFuzzyMap space fuzzyLeft 

fuzzyFrontRight :: CrispVector
                -> MemberShipValue
fuzzyFrontRight = fuzzyIntersection'  fuzzyFront fuzzyRight

fuzzyFrontRightMap space = toFuzzyMap space fuzzyFrontRight

-- needed for spoonAddFronLeftFar example
fuzzyFrontLeft :: CrispVector
               -> MemberShipValue
fuzzyFrontLeft = fuzzyIntersection' fuzzyFront fuzzyLeft

fuzzyFrontLeftMap space= toFuzzyMap space fuzzyFrontLeft

fuzzyBackRight :: CrispVector
               -> MemberShipValue
fuzzyBackRight = fuzzyIntersection' fuzzyBack fuzzyRight               
fuzzyBackRightMap space = toFuzzyMap space fuzzyBackRight               

fuzzyBackLeft :: CrispVector
              -> MemberShipValue
fuzzyBackLeft = fuzzyIntersection' fuzzyBack fuzzyLeft

fuzzyBackLeftMap space = toFuzzyMap space fuzzyBackLeft
