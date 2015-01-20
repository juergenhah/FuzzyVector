module ParallelVectors where

import qualified Control.Concurrent.Thread as Thread ( forkIO, result )
import FuzzyVector
import Distributions
import Control.Monad

{-main = do (tid, wait) <- Thread.forkIO $ do  x<- mapM (\a-> (a,myFuzzyAngle myXAxis myYAxis universe a))  [0,1..10] -- 90
                                               return x
            putStrLn "hallo"
            x <- Thread.result =<< wait
            return $ putStrLn $ show x
-}
