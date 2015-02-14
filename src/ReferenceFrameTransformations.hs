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
           -> FuzzyVector  -- ^ fuzzy ground template 
           -> FuzzyVector  -- ^ fuzzy ground object
           -> FuzzyVector  -- ^ fuzzy vector ort from
           -> FuzzyVector  -- ^ fuzzy vector ort to
           -> FuzzyVector  -- ^ fuzzy vector ofig
           -> FuzzyVector  -- ^ transformed vector
transform space gtpl ogrd vortfrom vort2 ofig = scaleAasBtoC space ogrd gtpl $!  do
  let !orientation=  (fuzzySub space vort2 vortfrom ) 
      !objectCentered = (fuzzySub space ofig ogrd )  
  sol <- runEval $! do
   orientationAngle <- rpar (force (fuzzyAngleofFuzzyVector space orientation )) 
   rseq orientationAngle
   o <- rpar (force (fuzzyRotation space objectCentered orientationAngle )) 
   rseq o
   return $! o
  return sol      

-- | checks if a fuzzy vector is inside all fuzzy spatial templates
satisfies :: DiscreteSpace            -- ^ space to work on
          -> FuzzyVector              -- ^ fuzzy vector to test
          -> [(String, FuzzyVector)]  -- ^ (fuzzy spatial template name, fuzzy spatial template) list
          -> Double                   -- ^ threshold value
          -> [(String,Bool)]          -- ^ (fuzzy spatial template name, True for threshold value is lowe, False for threshold value not reached from fuzzy vector)
satisfies space oj fis n = map (\fi-> (fst fi, fuzzyThresholdTest space (fuzzyIntersection oj (snd fi) ) n)) fis          
