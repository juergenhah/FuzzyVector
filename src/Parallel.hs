
module Parallel where

import Discretization
import Control.Parallel.Strategies
import Control.DeepSeq
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as N

discreteSpace = S {minX= (-30.0) ,minY= (-30.0)  ,maxX =30.0 ,maxY=30.0 ,stepsize=1} 
discreteSpace1 = S {minX= (-10.0) ,minY= (-10.0)  ,maxX =10.0 ,maxY=10.0 ,stepsize=1} 


m = createDescriteVectorsForBinaryOps discreteSpace


main ::IO ()
main  = do
 let vects = createDescriteVectorsForBinaryOps discreteSpace
     (as,bs)= splitAt (length vects `div` 2) vects
     sol = runEval $ do
           as' <-rpar (force (map  ((V.map (+0.1)).fst  ) as))
           bs' <-rpar (force (map  ((V.map (+0.2)).snd ) bs))
           rseq as'
           rseq bs'
           return (as'++bs')
 print sol          
