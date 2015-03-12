{-# LANGUAGE BangPatterns #-}
{-|
Module      : ReferenceFrameTransformations
Description : functions to transform fuzzy vectors to a different spatial reference fram
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

-}
module ReferenceFrameTransformations where

import Control.DeepSeq
import Control.Parallel.Strategies

import BinaryFuzzyOperation
import Discretization
import FuzzyVector

-- | transforms a fuzzy vector into another spatial reference frame
transform :: DiscreteSpace -- ^ space to work on
           -> FuzzyMap  -- ^ fuzzy ground template 
           -> FuzzyMap  -- ^ fuzzy ground object
           -> FuzzyMap  -- ^ fuzzy vector ort from
           -> FuzzyMap  -- ^ fuzzy vector ort to
           -> FuzzyMap  -- ^ fuzzy vector ofig
           -> FuzzyMap  -- ^ transformed vector
transform space gtpl ogrd vortfrom vort2 ofig = scaleAasBtoC space ogrd gtpl s   
 where s = fuzzyRotation space objectCentered orientationAngle
       !orientationAngle = fuzzyAngleofFuzzyVector space orientation
       !orientation=  (fuzzySub space vort2 vortfrom ) 
       !objectCentered = (fuzzySub space ofig ogrd )  

-- | checks if a fuzzy vector is inside all fuzzy spatial templates
satisfies :: DiscreteSpace            -- ^ space to work on
          -> FuzzyMap              -- ^ fuzzy vector to test
          -> [(String, FuzzyMap)]  -- ^ (fuzzy spatial template name, fuzzy spatial template) list
          -> Double                   -- ^ threshold value
          -> [(String,Bool)]          -- ^ (fuzzy spatial template name, True for threshold value is lowe, False for threshold value not reached from fuzzy vector)
satisfies space oj fis n = map (\fi-> (fst fi, fuzzyThresholdTest space (fuzzyIntersection oj (snd fi) ) n)) fis          

