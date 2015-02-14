{-|
Module      : GnuplotExport
Description : creates GnuPlot diagrams of a fuzzy vector
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 
 
-}
module GnuplotExport ( makeInteractiveMesh
                     , makePngMesh
                     , makePngMap
                     , exportFuzzyVectorstoPng
                     , showFuzzyVectors) where

import qualified Data.Vector.Storable as V
import           Data.List.Split
import           Data.List
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import           System.Exit
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PNG as Png

import           Discretization
import           FuzzyVector

-- | generates an interactive gnuplot window of a fuzzy vector
makeInteractiveMesh :: DiscreteSpace -- ^ space to work on 
                    -> FuzzyVector   -- ^ fuzzy vector to visualize
                    -> IO ExitCode   -- ^ did it work?
makeInteractiveMesh space mu = GP.plot X11.cons $
                               Frame.cons options $ 
                               Plot3D.mesh $ createMeshData space mu                     
 where options = (Opts.zLabel "membership" $ Opts.xLabel "X - Axis" $ Opts.grid True$ Opts.title "fuzzy vector membership" $ Opts.yLabel "Y - Axis"$ Opts.view 47.0 23.0 1.0 1.0 $  defltOpts)

-- | create default options
defltOpts :: Graph.C graph => Opts.T graph
defltOpts =
   Opts.key False $
   Opts.deflt       
   
-- | exports a png mesh of a fuzzy vector 
makePngMesh :: DiscreteSpace -- ^ space to work on
            -> FilePath      -- ^ filename of the exported mesh, default as png format
            -> FuzzyVector   -- ^ fuzzy vector to visualize
            -> IO ExitCode   -- ^ did it work?
makePngMesh space filename mu = GP.plot (Png.transparent $ Png.cons (filename++ ".png"))  
                                $ Frame.cons options $  Plot3D.mesh $
                                createMeshData space mu                     
 where options = ( Opts.view 47.0 23.0 1.0 1.0 $ Opts.grid True 
                 $ Opts.title filename $ Opts.xLabel "X - Axis"
                 $ Opts.yLabel "Y - Axis"$ Opts.zLabel "membership"  $ defltOpts)

-- | exports a png map of a fuzzy vector
makePngMap :: DiscreteSpace -- ^ space to work on
           -> FilePath      -- ^ name of the gnuplot export, default is png
           -> FuzzyVector   -- ^ fuzzy vector to visualize
           -> IO ExitCode   -- ^ did it work?
makePngMap space filename mu = GP.plot (Png.transparent $ Png.cons (filename++ ".png")) $
                               Frame.cons options $ 
                               Plot3D.mesh $ createMeshData space mu
 where  options = (Opts.grid True $ Opts.title filename $Opts.viewMap $ defltOpts)

-- | iterates over the space and creates a list of (X,Y,membership value) tuples
createMeshData :: DiscreteSpace              -- ^ space to work on
               -> FuzzyVector                -- ^ fuzzy vector to create data
               -> [[(Double,Double,Double)]] -- ^ (X,Y, membershipvalue) of the fuzzy vector
createMeshData space mu = map (map (\v -> (V.head v, (V.head . V.drop 1) v,mu v))) chuncks
 where len =round( 1+(maxX space-minX space)*(1/stepsize space))
       chuncks = chunksOf len $ createVectorDomain space

-- | export a list of fuzzy vectors as png files
exportFuzzyVectorstoPng :: DiscreteSpace -- ^ space to work on
                        -> [FilePath]    -- ^ list of filenames
                        -> [FuzzyVector] -- ^ list of fuzzy vectors to visualize
                        -> IO ()         -- ^ 
exportFuzzyVectorstoPng space filename fuzzyVector= sequence_ $ zipWith (makePngMesh space) filename fuzzyVector

-- | create interactive gnuplot windows for a list of fuzzy vectors
showFuzzyVectors :: DiscreteSpace -- ^ space to work on
                 -> [FuzzyVector] -- ^ list of fuzzy vectors to visualize
                 -> IO ()         -- ^
showFuzzyVectors  space fuzzyVector= mapM_ (makeInteractiveMesh space)  fuzzyVector
