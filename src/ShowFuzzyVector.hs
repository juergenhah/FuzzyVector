{-|
Module      : ShowFuzzyVector
Description : Functions for pretty printing a fuzzy vector on stdout
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

-}
module ShowFuzzyVector ( showDistribution
                       , fuzzyVectorToFile
                       , fromFileTofuzzyVector) where

import           Data.List
import           Data.List.Split
import qualified Data.Map as Map
import           Data.Maybe
import           System.IO.Extra
import           System.IO.Unsafe
import           Text.Printf

import           CrispVector
import           Discretization
import           FuzzyVector

type Distribution= [MemberShipValue]

-- | prints a distribution on the command line 
showDistribution :: Distribution -- ^ membership values for a fuzzy vector
                 -> DiscreteSpace -- ^ discrete space 
                 -> IO () -- ^ prints the distribution on the stdout
showDistribution dat space = mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks
  where len =round( 1+(maxX space-minX space)*(1/stepsize space))
        chuncks = chunksOf len dat

printdiscreteMembership :: (Distribution,String,DiscreteSpace)
                        -> IO()
printdiscreteMembership (dat,name,s) =  do
 let len =round( 1+(maxX s-minX s)*(1/stepsize s))
     chuncks =  chunksOf len dat
 putStrLn $ "membership function for: " ++ name
 putStrLn $ "row x length: " Prelude.++ (show len)
 mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks



createData :: DiscreteSpace           -- ^ space to work on
           -> FuzzyMap                -- ^ fuzzy vector to create data
           -> [(CrispVector,Double)] -- ^ (X,Y, membershipvalue) of the fuzzy vector
createData space mu = Map.toList mu

fuzzyVectorToFile :: DiscreteSpace
                  -> FuzzyMap
                  -> FilePath
                  -> IO ()
fuzzyVectorToFile space mu name = writeFile (name++ ".txt")  (show (dat))
 where dat = createData space mu

fromFileTofuzzyVector :: FilePath
                      -> FuzzyMap
fromFileTofuzzyVector name = Map.fromList list
 where list  = (read) .unsafePerformIO .readFile' $ (name++ ".txt")                      

