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
                     , showFuzzyVectors
                     , export3D
                     , exportMap
                     , testGnuPlot
                     , exportMapTupel
                     ) where


import           Data.List
import           Data.List.Split
import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Terminal.PNG as Png
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import           System.Directory
import           System.Exit
import qualified Graphics.Gnuplot.Terminal.Default as Term
import qualified Data.Map as Map
import           CrispVector
import           Discretization
import           FuzzyVector

testGnuPlot :: DiscreteSpace -> FuzzyMap-> IO ()
testGnuPlot space mu= 
  putStr $ fst $ GP.fileContents "test" Term.cons  $  Frame.cons options $ 
  Plot3D.mesh $ createMeshData space mu                    
 where options =  (
                    Opts.add (Opt.custom "xzeroaxis" "") ["lw","4","lt","1","lc","rgb","'white'"]  $ 
                    Opts.add (Opt.custom "yzeroaxis" "") ["lw","4","lt","1","lc","rgb","'white'"]  $ 
                    Opts.add (Opt.custom "grid" "xtics ytics") ["ls","2","lw","1","lt","4","lc","rgb","'white'"] $  
                    Opts.add (Opt.custom "xlabel" "") ["\"xlabel\"", "font","\"{Verdana},{30}\""]  $ Opts.add (Opt.custom "xtics" "") ["font", "\"{Verdana},{20}\""]$
                    Opts.add (Opt.custom "clabel" "") ["\"membership\"","font","\"Verdana,20\""]  $          Opts.add (Opt.custom "xtics" "") ["font", "\"Verdana,20\""]  $
                    Opts.viewMap  $ 
                    Opts.yLabel "Y - Axis"$ 
                    Opts.view 47.0 23.0 1.0 1.0 $  defltOpts)

-- | generates an interactive gnuplot window of a fuzzy vector
makeInteractiveMesh :: DiscreteSpace -- ^ space to work on 
                     -> FuzzyMap   -- ^ fuzzy vector to visualize
                     -> IO ExitCode   -- ^ did it work?
makeInteractiveMesh space mu = GP.plot X11.cons $
                                Frame.cons options $ 
                                Plot3D.mesh $ createMeshData space mu                    
 where options =  (
                   Opts.add (Opt.custom "xzeroaxis" "") ["lw","4","lt","1","lc","rgb","'white'"]  $ 
                   Opts.add (Opt.custom "yzeroaxis" "") ["lw","4","lt","1","lc","rgb","'white'"]  $ 
                   Opts.add (Opt.custom "grid" "xtics ytics") ["ls","2","lw","1","lt","4","lc","rgb","'white'"] $  
                   Opts.add (Opt.custom "xlabel" "") ["\"xlabel\""," font \"Verdana,30\""]  $
                   Opts.viewMap  $ 
                   Opts.yLabel "Y - Axis"$ 
                   Opts.view 47.0 23.0 1.0 1.0 $  defltOpts)

-- | create default options
defltOpts :: Graph.C graph => Opts.T graph
defltOpts =
   Opts.key False $!
   Opts.deflt       
   
-- | exports a png mesh of a fuzzy vector 
makePngMesh :: DiscreteSpace -- ^ space to work on
            -> FilePath      -- ^ filename of the exported mesh, default as png format
            -> FuzzyMap   -- ^ fuzzy vector to visualize
            -> IO ExitCode   -- ^ did it work?
makePngMesh space filename mu = GP.plot (Png.transparent $ 
                                         Png.fontLarge $
                                         Png.cons (filename++ ".png")
                                         )  
                                         $ Frame.cons options 
                                         $  Plot3D.mesh $!
                                         createMeshData space mu                     
 where options = (Opts.add (Opt.custom "xzeroaxis" "") ["lw","1.5","lt","1","lc","rgb","'white'"]  $ 
                   Opts.add (Opt.custom "yzeroaxis" "") ["lw","1.5","lt","1","lc","rgb","'white'"]  $ 
                   Opts.add (Opt.custom "grid" "xtics ytics") ["lw","1","lt","0","lc","rgb","'white'"] $  
                   Opts.view 47.0 23.0 1.0 1.0 $ Opts.grid True $
                   Opts.title filename $ Opts.xLabel "X - Axis" $
                   Opts.yLabel "Y - Axis"$ Opts.zLabel "membership"  $ defltOpts)

-- | exports a png map of a fuzzy vector
makePngMap :: DiscreteSpace -- ^ space to work on
           -> FilePath      -- ^ name of the gnuplot export, default is png
           -> FuzzyMap   -- ^ fuzzy vector to visualize
           -> IO ExitCode   -- ^ did it work?
makePngMap space filename mu = GP.plot (Png.transparent $ --Png.fontGiant $
 Png.cons (filename++ ".png")) $
                               Frame.cons options $ 
                               Plot3D.mesh $! createMeshData space mu
 where  options = (Opts.add (Opt.custom "xzeroaxis" "") ["lw","4","lt","1","lc","rgb","'white'"]  $       
                    Opts.add (Opt.custom "yzeroaxis" "") ["lw","4","lt","1","lc","rgb","'white'"]  $       
                    Opts.add (Opt.custom "grid" "xtics ytics") ["ls","13", "lw","2","lt","0","lc","rgb","'white'"] $   
        
                    Opts.add (Opt.custom "xtics" "") ["font", "\"Verdana,20\""]  $
                    Opts.add (Opt.custom "ytics" "") ["font", "\"Verdana,20\""]  $
                    Opts.add (Opt.custom "cbtics" "") ["font", "\"Verdana,20\""] $
                    Opts.add (Opt.custom "colorbox" "") ["noborder"]  $         
                    Opts.viewMap $ defltOpts)

-- | iterates over the space and creates a list of (X,Y,membership value) tuples
createMeshData :: DiscreteSpace              -- ^ space to work on
               -> FuzzyMap                -- ^ fuzzy vector to create data
               -> [[(Double,Double,Double)]] -- ^ (X,Y, membershipvalue) of the fuzzy vector
createMeshData space mu = chunksOf len. map (\(v,m) -> (getX v,getY v,m)). Map.toList $ mu
 where len =round( 1+(maxX space-minX space)*(1/stepsize space))
       chuncks = chunksOf len $ createVectorDomain space

-- | export a list of fuzzy vectors as png files
exportFuzzyVectorstoPng :: DiscreteSpace -- ^ space to work on
                        -> [FilePath]    -- ^ list of filenames
                        -> [FuzzyMap] -- ^ list of fuzzy vectors to visualize
                        -> IO ()         -- ^ 
exportFuzzyVectorstoPng space filename fuzzyVector= sequence_ $! zipWith (makePngMesh space) filename fuzzyVector

exportFuzzyVectorstoPngMap :: DiscreteSpace -- ^ space to work on
                           -> [FilePath]    -- ^ list of filenames
                           -> [FuzzyMap] -- ^ list of fuzzy vectors to visualize
                           -> IO ()         -- ^ 
exportFuzzyVectorstoPngMap space filename fuzzyVector= sequence_ $! zipWith (makePngMap space) filename fuzzyVector

-- | create interactive gnuplot windows for a list of fuzzy vectors
showFuzzyVectors :: DiscreteSpace -- ^ space to work on
                 -> [FuzzyMap] -- ^ list of fuzzy vectors to visualize
                 -> IO ()         -- ^
showFuzzyVectors  space fuzzyVector= mapM_ (makeInteractiveMesh space)  fuzzyVector


export3D :: DiscreteSpace
       -> [String]
       -> String
       -> [FuzzyMap]
       -> IO()
export3D space names folder vectors = do 
 createDirectoryIfMissing True folder
 let newnames = map (\n->filepath++n++"3D") names
     filepath = "./"++folder++"/"
 exportFuzzyVectorstoPng space newnames vectors

exportMap :: DiscreteSpace
          -> [String]
          -> String
          -> [FuzzyMap]
          -> IO()
exportMap space names folder vectors = do 
 createDirectoryIfMissing True folder
 let newnames = map (\n->filepath++n++"Map") names
     filepath = "./"++folder++"/"
 exportFuzzyVectorstoPngMap space newnames vectors

exportMapTupel :: DiscreteSpace
               -> String
               -> [(String,FuzzyMap)]
               -> IO ()
exportMapTupel space folder vecs =exportMap space names folder m
 where names = map fst vecs
       m = map snd vecs              
