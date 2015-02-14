{-|
Module      : LatexExport
Description : functions to export data in a format which can be used for Latex tikz
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 
 
-}
module LatexExport ( convertScaleFactor4tikz
                   , getZeroProbAngles
                   , getNonZeroProbAngles
                   ) where

import Data.List

import Discretization
import FuzzyVector

-- | transforms a fuzzy scale factor lookup list to a format useable for tikz
convertScaleFactor4tikz :: [(ScaleFactor,MemberShipValue)] -- ^ fuzzy scale factor lookup list
                        -> IO ()
convertScaleFactor4tikz = mapM_ (\(a,v)-> putStr $ "("++ show a ++ ","++show v++") ") 

-- | get only membership values that equal zero of the fuzzy vector
getZeroProbAngles :: DiscreteSpace -- ^ space to work on  
                  -> FuzzyVector   -- ^ fuzzy vector
                  -> IO ()         -- ^ values to copy to Latex
getZeroProbAngles  space vec = mapM_ (\(a,v)-> putStr $ "("++ show a ++ ", 1.0"++") ") . sort .filter ((==0).snd). map (\angle -> (angle,fuzzyAngleofFuzzyVector space vec angle) ) $ createAngleDomain space

-- | get only membership values that are higher than 0 of the fuzzy 
getNonZeroProbAngles :: DiscreteSpace -- ^ space to work on 
                     -> FuzzyVector   -- ^ fuzzy vector
                     -> IO ()         -- ^ values to copy to Latex
getNonZeroProbAngles  space vec = mapM_ (\(a,v)-> putStr $ "("++ show a ++ ","++show v++") ") . sort .filter ((>0).snd). map (\angle -> (angle,fuzzyAngleofFuzzyVector space vec  angle) ) $createAngleDomain space
