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

-- * fuzzy ground templates

fuzzyGroundTemplate :: CrispVector
                    -> MemberShipValue
fuzzyGroundTemplate = fuzzyCircleTemplate 3 0 (crispVector 0.0 0.0)

bigfuzzyGroundTemplate :: CrispVector
                       -> MemberShipValue
bigfuzzyGroundTemplate = fuzzyCircleTemplate 5 0 (crispVector 0.0 0.0)

-- * fuzzy spatial templates
allSpatialTemplates = [fuzzyHere,fuzzyNear,fuzzyFar,fuzzyVeryFar,fuzzyFront,fuzzyBack,fuzzyRight,fuzzyLeft,
                                fuzzyFrontRight,fuzzyFrontLeft,fuzzyBackRight,fuzzyBackLeft]
allSpatialTemplateNames = ["here","near","far","veryfar","front","back","right","left"
                                         ,"front-right","front-left","back-right","back-left"]                

namedSpatialTemplates = zipWith (\a b -> (a,b)) allSpatialTemplateNames allSpatialTemplates

fuzzyHere :: CrispVector 
            -> MemberShipValue
fuzzyHere = fuzzyCircleTemplate 1 1 (crispVector 0.0 0.0)

-- near
fuzzyNear :: CrispVector 
          -> MemberShipValue
fuzzyNear = fuzzyComplement (fuzzyCircleTemplate 2 1 (crispVector 0.0 0.0))  fuzzyHere

-- far
fuzzyFar :: CrispVector 
         -> MemberShipValue
fuzzyFar = fuzzyComplement (fuzzyCircleTemplate 4 1 (crispVector 0.0 0.0))  (fuzzyUnion fuzzyNear fuzzyHere) 

fuzzyVeryFar :: CrispVector
             -> MemberShipValue
fuzzyVeryFar = fuzzyComplement (fuzzyCircleTemplate 6 2 (crispVector 0.0 0.0))  (fuzzyUnion fuzzyFar $ fuzzyUnion fuzzyNear fuzzyHere)       


fuzzyFront :: CrispVector 
           -> MemberShipValue
fuzzyFront = fuzzyDirectionTemplate (300) (60) 5 

fuzzyBack :: CrispVector
          -> MemberShipValue
fuzzyBack = fuzzyDirectionTemplate (120) (240) 5  

fuzzyRight :: CrispVector
           -> MemberShipValue
fuzzyRight = fuzzyDirectionTemplate (30) (150) 5  

fuzzyLeft :: CrispVector
          -> MemberShipValue
fuzzyLeft = fuzzyDirectionTemplate (210) (330) 5  

fuzzyFrontRight :: CrispVector
                -> MemberShipValue
fuzzyFrontRight = fuzzyIntersection  fuzzyFront fuzzyRight

fuzzyFrontLeft :: CrispVector
               -> MemberShipValue
fuzzyFrontLeft = fuzzyIntersection fuzzyFront fuzzyLeft

fuzzyBackRight :: CrispVector
               -> MemberShipValue
fuzzyBackRight = fuzzyIntersection fuzzyBack fuzzyRight                              

fuzzyBackLeft :: CrispVector
              -> MemberShipValue
fuzzyBackLeft = fuzzyIntersection fuzzyBack fuzzyLeft
