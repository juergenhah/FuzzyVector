{-|
Module      : Main
Description : creates all figures that are included into the COSIT 2015 article
Copyright   : (c) Juergen Hahn, 2015
                  Simon Scheider, 2015
License     : GPL-3
Maintainer  : hahn@geoinfo.tuwien.ac.at
Stability   : experimental
Portability : 

-}
module Main where

import           Control.DeepSeq
import           Control.Monad.Trans
import           Control.Parallel.Strategies
import           Data.List
import           Data.Maybe
import           System.Directory
import qualified Data.Map as Map

import           BinaryFuzzyOperation
import           Discretization
import           FuzzyExamples
import           FuzzyVector
import qualified GnuplotExport as GnuPlot
import           ReferenceFrameTransformations
import           ShowFuzzyVector
import           SpatialTemplates

import qualified CrispVector as C

-- * Figures for Paper
-- compile via: cabal install or  ghc -O2 Main.hs -rtsopts -threaded -H128m
-- run via:./ReferenceFrame or  ./Main a.txt +RTS -N -s -h

-- | space definition for discretization
discreteSpace = S {minX = (-20.0)
                  ,minY = (-20.0)
                  ,maxX = 20.0 
                  ,maxY = 20.0 
                  ,stepsize = 1
                  }   


-- | calls functions to produce figures for the COSIT paper
main:: IO ()
main = do
 let space = discreteSpace
     filepath = "figures/spatialTransformations"
 putStrLn "start with the figures"
 createFiguresForPaper discreteSpace
 putStrLn "\n** export of example figures finished **\n"
 calctransforms discreteSpace
 putStrLn "\n** export of spatial reference frame transformation finished **\n" 
 satisfiesTransformations
 putStrLn "\n** do scaled spatial templates satisfy the transformation, finished **\n" 
 exportSpatialTemplates
 putStrLn "\n** export of spatial templates finished **\n" 

-- ** figures to explain the fuzzy operations
createFiguresForPaper :: DiscreteSpace
                      ->  IO ()
createFiguresForPaper space = do
 putStrLn "adding spoon to front left far"
 spoonAddFrontLeftFar space
 putStrLn "\nsubtracting person from house"
 personSubhouse space
 putStrLn "\nrotating Spoon with fuzzy angle calculated from objA to objA"
 rotateSpoonWithB space
 putStrLn "\nscale fuzzy spoon2 with scale factor from objA to big fuzzy ground template"
 scaleFuzzyVector space
 putStrLn "\ntransformations"


-- | example for fuzzy addition
spoonAddFrontLeftFar :: DiscreteSpace
                     -> IO ()
spoonAddFrontLeftFar space = do
 let filepath = "figures/spoonAddFronLeftFar"  
 createDirectoryIfMissing True filepath  
 let figure1=  fuzzyAdd space fuzzyRealSpoon int 
     figure11 = fuzzyAdd space fuzzyRealSpoon fuzzyFrontLeft 
     int = (fuzzyIntersectionMap fuzzyFrontLeft fuzzyFar )
     fuzzyRealSpoon = fuzzyRealSpoonMap space
     fuzzyFrontLeft = fuzzyFrontLeftMap space
     fuzzyFar = fuzzyFarMap space
 fuzzyVectorToFile space (figure1) ("./"++ filepath++"/" ++"fuzzy_spoon_added_fuzzy_front_left_far")    
 fuzzyVectorToFile space (figure11) ("./"++ filepath++"/" ++"fuzzy_spoon_added_fuzzy_front_left")
 sequence_ [GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_spoon") fuzzyRealSpoon
            ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_front_left") fuzzyFrontLeft
            ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_far") fuzzyFar
            ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_front_left_far") int
            ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_spoon_added_fuzzy_front_left_far") figure1          
            ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_spoon_added_fuzzy_front_left") figure11
            ] 

-- | example for a fuzzy subtraction
personSubhouse :: DiscreteSpace
               -> IO ()
personSubhouse space = do
 let filepath = "figures/personSubHouse"  
 createDirectoryIfMissing True filepath
 let figure = fuzzySub space person4quadrant house4quadrant 
     person4quadrant = person4quadrantMap space
     house4quadrant = house4quadrantMap space
 fuzzyVectorToFile space (figure) ("./"++ filepath++"/" ++"person4quadrant_sub_house4quadrant")
 sequence_ [ GnuPlot.makePngMap space ("./"++ filepath++"/" ++"person4quadrant") person4quadrant
           , GnuPlot.makePngMap space ("./"++ filepath++"/" ++"house4quadrant") house4quadrant
           , GnuPlot.makePngMap space ("./"++ filepath++"/" ++"person4quadrant_sub_house4quadrant") figure]

-- | example for a fuzzy rotation

rotateSpoonWithB :: DiscreteSpace
                 -> IO ()
rotateSpoonWithB  space = do
 let filepath = "figures/rotateSpoonWithObjB"
 createDirectoryIfMissing True filepath
 let fuzzyAngle =fuzzyAngleofFuzzyVector space objB 
     rotated = fuzzyRotation space fuzzyRealSpoon  fuzzyAngle
     fuzzyRealSpoon = fuzzyRealSpoonMap space
     objB = objBMap space
 fuzzyVectorToFile space (rotated) ("./"++ filepath++"/" ++"fuzzy_spoon_rotated_by_fuzzy_angle_of_obj_B")    
 sequence_ [GnuPlot.makePngMap space ("./"++ filepath++"/" ++"obj_B")  objB
           ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_spoon") fuzzyRealSpoon
           ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"fuzzy_spoon_rotated_by_fuzzy_angle_of_obj_B") rotated
           ]
 putStrLn "\nvalues for pgfplot polar for zero values"           
 --getZeroProbAngles space objB            
 putStrLn "\nvalues for pgfplot polar for NON zero values"           
-- getNonZeroProbAngles  space objB

-- | example for fuzzy Scaling
scaleFuzzyVector :: DiscreteSpace -- ^ space to work on
                 -> IO ()
scaleFuzzyVector space = do
 let filepath = "figures/fuzzyScaling"
 createDirectoryIfMissing True filepath
 let sf = lookupScale sf'
     sf' = calculateFuzzyScaleFactor' space centerobjA1 bigfuzzyGroundTemplate 
     centerobjA1 = translateToCenterWithCentroid space objA 
     crispScaleFactor = calcCentroidFuzzyScaleFactor space sf
     scaled = fuzzyCentroidScale space fuzzyRealSpoon  sf 
     objA = objAMap space
     fuzzyRealSpoon = fuzzyRealSpoonMap space
     bigfuzzyGroundTemplate = bigfuzzyGroundTemplateMap space
 putStrLn "scale factors for paper:"
 print  sf'    
 putStrLn $ "Crisp Scale Factor:"++ show  crispScaleFactor
 fuzzyVectorToFile space (scaled) ("./"++ filepath++"/" ++"scaled_real_spoon")    
 sequence_ [GnuPlot.makePngMap space ("./"++ filepath++"/" ++"obj_A") objA
           ,GnuPlot.makePngMap space ("./"++ filepath++"/" ++"centered_ obj_A") centerobjA1
           ,GnuPlot.makePngMap space  ("./"++ filepath++"/" ++ "bigfuzzyGroundTemplate_union_obj_A") $ fuzzyUnionMap objA bigfuzzyGroundTemplate
           ,GnuPlot.makePngMap space  ("./"++ filepath++"/" ++"bigfuzzyGroundTempalte") bigfuzzyGroundTemplate  
           ,GnuPlot.makePngMap space  ("./"++ filepath++"/" ++"fuzzy_real_spoon") fuzzyRealSpoon
           ,GnuPlot.makePngMap space  ("./"++ filepath++"/" ++"scaled_real_spoon") scaled    ]

-- * spatial reference transformations

-- | examples for spatial reference frames and how to transform them
calctransforms :: DiscreteSpace
               -> IO ()
calctransforms space = do
 let filepath = "figures/spatialTransformations"
 createDirectoryIfMissing True filepath  
 putStrLn "\n FIGURE showing the spatial configuration  -----"
 GnuPlot.makePngMap space ("./"++ filepath++"/" ++"spatialConfiguration") $ spatialConfiguration
 fuzzyVectorToFile space spatialConfiguration ("./"++ filepath++"/" ++"spatialConfiguration")
-- putStrLn $ show  satfiga
 putStrLn "\n FIGURE for egocentrice absolute  -----"
 GnuPlot.makePngMap space ("./"++ filepath++"/" ++"egocentricAbsolute") $ egocentric space
 fuzzyVectorToFile space (egocentric space) ("./"++ filepath++"/" ++"egocentricAbsolute")
-- putStrLn $ show  satfigb
 putStrLn "\n FIGURE allocentric -----"
 GnuPlot.makePngMap space ("./"++ filepath++"/" ++"allocentric") $allocentric space
 fuzzyVectorToFile space (allocentric space) ("./"++ filepath++"/" ++"allocentric")
-- putStrLn $ show  satfigc
 putStrLn "\n FIGURE intrinsic -----"
 GnuPlot.makePngMap space ("./"++ filepath++"/" ++"intrinsic") $intrinsic space
 fuzzyVectorToFile space (intrinsic space) ("./"++ filepath++"/" ++"intrinsic")
-- putStrLn $ show  satintrinsic
 putStrLn "\n FIGURE E -----"
 GnuPlot.makePngMap space ("./"++ filepath++"/" ++"deictic") $deictic space
 fuzzyVectorToFile space (deictic space) ("./"++ filepath++"/" ++"deictic")
-- putStrLn $ show  satfige 
 putStrLn "\n FIGURE F -----"
 GnuPlot.makePngMap space ("./"++ filepath++"/" ++"retinal") $retinal space
 fuzzyVectorToFile space (retinal space) ("./"++ filepath++"/" ++"retinal")
-- putStrLn $ show satfigf
 print "DONE"


-- | figure (a) from paper
spatialConfiguration = fuzzyUnionMap observer $ fuzzyUnionMap tree $ fuzzyUnionMap largehouse realLargehouseFront
 where realLargehouseFront = realLargehouseFrontMap space
       observer = observerMap space
       tree = treeMap space
       largehouse = largehouseMap space
       space = discreteSpace 

iGnuPlotSpatialConfiguration = GnuPlot.makeInteractiveMesh discreteSpace spatialConfiguration

-- | figure (b) from paper
egocentric :: DiscreteSpace 
           -> FuzzyMap
egocentric space = fuzzyUnionMap observer 
                 $ fuzzyUnionMap rottree 
                 $ fuzzyUnionMap rothouse rothouseFront
 where fuzzyAngle = fuzzyAngleofFuzzyVector discreteSpace north
       rottree = fuzzyRotation discreteSpace tree fuzzyAngle 
       rothouse = fuzzyRotation discreteSpace largehouse  fuzzyAngle
       rothouseFront =fuzzyRotation discreteSpace realLargehouseFront  fuzzyAngle
       tree = treeMap space
       largehouse = largehouseMap space
       north = northMap space
       realLargehouseFront = realLargehouseFrontMap space
       observer = observerMap space

-- | figure (c) from paper
allocentric :: DiscreteSpace
            -> FuzzyMap
allocentric space =  let angle = fuzzyAngleofFuzzyVector space  north
                         subtraction = fuzzySub space tree largehouse 
                         north = northMap space
                         tree = treeMap space
                         largehouse = largehouseMap space
 in  fuzzyRotation space subtraction angle



-- | figure (d) from paper
intrinsic :: DiscreteSpace
          -> FuzzyMap
intrinsic space = fuzzyRotation space (fuzzySub discreteSpace tree largehouse  )  fuzzyAngleFront   
 where fuzzyFront = fuzzySub space largehouseFront house 
       fuzzyAngleFront = fuzzyAngleofFuzzyVector space fuzzyFront 
       largehouseFront = largehouseFrontMap space
       tree = treeMap space
       largehouse = largehouseMap space
       house = houseMap space


-- | figure (e) from paper
deictic :: DiscreteSpace
        -> FuzzyMap
deictic space = fuzzySub space tree largehouse
 where tree = treeMap space
       largehouse = largehouseMap space


-- | figure (f) from paper
retinal :: DiscreteSpace
        -> FuzzyMap
retinal space= fuzzyRotation discreteSpace subtraction  angle   
 where angle = fuzzyAngleofFuzzyVector discreteSpace frontdirection 
       frontdirection = fuzzySub discreteSpace observer largehouse
       subtraction = fuzzySub discreteSpace tree largehouse 
       tree = treeMap space
       observer = observerMap space
       largehouse = largehouseMap space
      
      
     
toGnuPlotSatfig = GnuPlot.exportMap discreteSpace  allSpatialTemplateNames "/figures/scaledfigf" scaledRefFrames
 where scaledRefFrames = map (\ (_,nframe)  ->scaleAasBtoC discreteSpace nframe fuzzyGroundTemplate largehouse  )  (namedSpatialTemplates space)
       fuzzyGroundTemplate = fuzzyGroundTemplateMap space
       largehouse = largehouseMap space
       space = discreteSpace

-- * satisfies functions

satisfiesTransformations :: IO ()
satisfiesTransformations = do
 let readSpatialConfig = readFuzzyVector "spatialConfiguration"
     minsat = 0.8
 putStrLn "\n satisfies figure a"
 print .filter ((==True).snd) $satisfies discreteSpace readSpatialConfig (allNamedST)  minsat
 let readEgoAbsolut= readFuzzyVector "egocentricAbsolute"
 putStrLn "\n satisfies figure b"
 print .filter ((==True).snd)$ satisfies discreteSpace (readEgoAbsolut) (allNamedST)   minsat
 let readAllocentric = readFuzzyVector "allocentric"
 putStrLn "\n satisfies figure c" --not scaled??
 print .filter ((==True).snd)$ satisfies discreteSpace (readAllocentric) scaledST  minsat
 let readIntrinsic = readFuzzyVector "intrinsic"
 putStrLn "\n satisfies figure d"
 print .filter ((==True).snd)$ satisfies discreteSpace (readIntrinsic) scaledST  minsat   
 let readDeictic = readFuzzyVector "deictic"
 putStrLn "\n satisfies figure e"
 print .filter ((==True).snd)$ satisfies discreteSpace (readDeictic) scaledST minsat
 let readRetinal = readFuzzyVector "retinal"
 putStrLn "\n satisfies figure f"
 print .filter ((==True).snd)$ satisfies discreteSpace (readRetinal) scaledST  minsat
 putStrLn "DONE"

toGnuPlotscaledST = do
 GnuPlot.exportMapTupel discreteSpace  "figures/scaledrefFrames/" (scaledST)

scaledST = map (\(name,nframe)  -> (name,scaleAasBtoC discreteSpace fuzzyGroundTemplate largehouse nframe ))  (allNamedST )
 where fuzzyGroundTemplate = fuzzyGroundTemplateMap space
       largehouse = largehouseMap space
       space = discreteSpace

readFuzzyVector name = fuzzyVector
 where  fuzzyVector =fromFileTofuzzyVector $ "./figures/spatialTransformations/"++ ( name)

-- 
allNamedST = namedHereST ++ namedNearST ++ namedFarST ++namedVeryFarST

namedHereST = toNamedTemplates allHereSpatialTemplates "here"
namedNearST = toNamedTemplates allNearSpatialTemplates "near"
namedFarST = toNamedTemplates allFarSpatialTemplates "far"
namedVeryFarST = toNamedTemplates allVeryFarSpatialTemplates "veryfar"

toNamedTemplates templates n= zipWith (\name templ -> (name++"_"++n,templ)) allSpatialTemplateNames templates


-- * export of spatial templates
exportSpatialTemplates :: IO()
exportSpatialTemplates = do
 putStrLn "exporting 'here' tempaltes "
 exportHereFigures
 putStrLn "exporting 'near' tempaltes "
 exportNearFigures
 putStrLn "exporting 'far' tempaltes "
 exportFarFigures
 putStrLn "exporting 'very far' tempaltes "
 exportVeryFarFigures

allHereSpatialTemplates = map (fuzzyIntersectionMap fuzzyHere) (allSpatialTemplates discreteSpace)
 where fuzzyHere = fuzzyHereMap discreteSpace
intHereSTem= GnuPlot.showFuzzyVectors discreteSpace allHereSpatialTemplates

exportHereFigures = do
 sequence_ [ pngHereSTem
           , pngMapHereSTem
           ]
pngHereSTem = GnuPlot.export3D discreteSpace allSpatialTemplateNames "figures/here" allHereSpatialTemplates

pngMapHereSTem = GnuPlot.exportMap discreteSpace allSpatialTemplateNames "figures/here" allHereSpatialTemplates

-- ** exports 'far' spatial templates
allNearSpatialTemplates = map (fuzzyIntersectionMap fuzzyNear) (allSpatialTemplates discreteSpace) 
 where fuzzyNear = fuzzyNearMap discreteSpace
intNearSTem= GnuPlot.showFuzzyVectors discreteSpace allNearSpatialTemplates

exportNearFigures = do
 sequence_ [ pngNearSTem
             , pngMapNearSTem
               ]
pngNearSTem = GnuPlot.export3D discreteSpace allSpatialTemplateNames "figures/near" allNearSpatialTemplates

pngMapNearSTem = GnuPlot.exportMap discreteSpace allSpatialTemplateNames "figures/near" allNearSpatialTemplates


-- ** exports 'far' spatial templates
allFarSpatialTemplates =  map (fuzzyIntersectionMap fuzzyFar) (allSpatialTemplates discreteSpace)
 where fuzzyFar = fuzzyFarMap discreteSpace 
intFarSTem= GnuPlot.showFuzzyVectors discreteSpace allFarSpatialTemplates

exportFarFigures = do
 sequence_ [ pngFarSTem
           , pngMapFarSTem
           ]
pngFarSTem = GnuPlot.export3D discreteSpace allSpatialTemplateNames "figures/far" allFarSpatialTemplates

pngMapFarSTem = GnuPlot.exportMap discreteSpace allSpatialTemplateNames "figures/far" allFarSpatialTemplates

-- ** exports 'very far' spatial templates
allVeryFarSpatialTemplates =  map (fuzzyIntersectionMap fuzzyVeryFar)  (allSpatialTemplates discreteSpace)
 where fuzzyVeryFar = fuzzyVeryFarMap discreteSpace
 


intVeryFarSTem= GnuPlot.showFuzzyVectors discreteSpace allVeryFarSpatialTemplates

exportVeryFarFigures = do
 sequence_ [ pngVeryFarSTem
           , pngMapVeryFarSTem
           ]
pngVeryFarSTem = GnuPlot.export3D discreteSpace allSpatialTemplateNames "figures/veryFar" allVeryFarSpatialTemplates

pngMapVeryFarSTem = GnuPlot.exportMap discreteSpace allSpatialTemplateNames "figures/veryFar" allVeryFarSpatialTemplates

-- | show all created Reference Frames
interReferenceFrame =GnuPlot.showFuzzyVectors discreteSpace (allSpatialTemplates discreteSpace) 



interactiveFuzzyVectors = GnuPlot.showFuzzyVectors discreteSpace fuzzyVectors
 where fuzzyVectors = [fuzzySpoonMap discreteSpace,fuzzyForkMap discreteSpace]

intSPatTemplate = GnuPlot.showFuzzyVectors discreteSpace (allSpatialTemplates discreteSpace) 

exportAllFuzzyVectors= GnuPlot.exportFuzzyVectorstoPng discreteSpace names fuzzyVectors
 where names = ["Spoon","Fork","NearRight","SpoonUnionFork"]
       fuzzyVectors = [fuzzySpoonMap space,fuzzyForkMap space,fuzzyBackRightMap space,fuzzySpoonUnionFork]
       fuzzySpoonUnionFork= fuzzyUnionMap fuzzySpoon fuzzyFork
       fuzzySpoon = fuzzySpoonMap space
       fuzzyFork = fuzzyForkMap space
       space = discreteSpace

