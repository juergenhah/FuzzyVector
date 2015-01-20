module Helper where
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N
import           Text.Printf
import           Data.List.Split
import Data.List
import Distributions
import FuzzyVector


printdiscreteMembership :: (Distribution,String,DiscreteSpace)->IO()
printdiscreteMembership (dat,name,s) =  do
 let len =round( 1+(maxX s-minX s)*(1/stepsize s))
     chuncks =  chunksOf len dat
 putStrLn $ "membership function for: " ++ name
 putStrLn $ "row x length: " Prelude.++ (show len)
 mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks

-- | prints a distribution on the command line 
showDistribution :: Distribution -> DiscreteSpace -> IO ()
showDistribution dat space = mapM_ putStr. intercalate ["\n"] . map (map (printf "%+-.2f ") ) $ chuncks
  where len =round( 1+(maxX space-minX space)*(1/stepsize space))
        chuncks = chunksOf len dat

-- * helper and printing functions
toGnuPlotDataFile :: FuzzyVector -> DiscreteSpace -> FilePath -> IO ()
toGnuPlotDataFile fuzzyVector space filename = do
 let len = round( 1+(maxX space-minX space)*(1/stepsize space))
     distribution = discretizeFuzzyVector space fuzzyVector
     chuncks =  chunksOf len distribution
 writeFile filename ""
 mapM_ (appendFile filename). intercalate ["\n"] . map (map (\n -> show n ++" ") ) $ chuncks


