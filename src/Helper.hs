

{-|
Module      : Helper
Description : Functions for pretty printing and export to make a GnuPlot figure
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

-}
 module Helper where
import qualified Data.Vector.Storable as V

import           Text.Printf
import           Data.List.Split
import Data.List

import Distributions
import Discretization


printdiscreteMembership :: (Distribution,String,DiscreteSpace)
                        -> IO()
printdiscreteMembership (dat,name,s) =  do
 let len =round( 1+(maxX s-minX s)*(1/stepsize s))
     chuncks =  chunksOf len dat
 putStrLn $ "membership function for: " ++ name
 putStrLn $ "row x length: " Prelude.++ (show len)
 mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks

-- | prints a distribution on the command line 
showDistribution :: Distribution 
                 -> DiscreteSpace 
                 -> IO ()
showDistribution dat space = mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks
  where len =round( 1+(maxX space-minX space)*(1/stepsize space))
        chuncks = chunksOf len dat

-- * helper and printing functions
toGnuPlotDataFile :: (CrispVector -> Probability) 
                  -> DiscreteSpace 
                  -> FilePath 
                  -> IO ()
toGnuPlotDataFile fuzzyVector space filename = do
 let len = round( 1+(maxX space-minX space)*(1/stepsize space))
     distribution = discretizeFuzzyVector space fuzzyVector
     chuncks =  chunksOf len distribution
 writeFile filename ""
 mapM_ (appendFile filename). intercalate ["\n"] . map (map (\n -> show n ++" ") ) $ chuncks


discretizeFuzzyVector :: DiscreteSpace 
                      -> (CrispVector -> Probability)  
                      -> Distribution
discretizeFuzzyVector space memFunc  = map memFunc .createDescriteVectorsForSpace $ space


