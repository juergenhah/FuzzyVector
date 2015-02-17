{-# LANGUAGE BangPatterns #-}
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

import           Control.Parallel.Strategies
import           Control.DeepSeq
import           Data.List
import  Control.Monad.Trans
import           BinaryFuzzyOperation
import           Discretization
import           FuzzyExamples
import           FuzzyVector
import qualified GnuplotExport as GnuPlot
import           LatexExport
import           ReferenceFrameTransformations
import           ShowFuzzyVector
import           SpatialTemplates

import qualified CrispVector as C



-- * Figures for Paper
-- compile using:  ghc -O2 Main.hs -rtsopts -threaded -H128m
-- run using: ./Main a.txt +RTS -N -s

-- | space definition for discretization
discreteSpace = S {minX = (-20.0)
                  ,minY = (-20.0)
                  ,maxX = 20.0 
                  ,maxY = 20.0 
                  ,stepsize = 1
                  }   

-- | calls functions to produce figures for the COSIT paper
main:: IO ()
main =do createFiguresForPaper 
         print  "finished"



-- ** figures to explain the fuzzy operations
createFiguresForPaper :: IO ()
createFiguresForPaper = do
 putStrLn "adding spoon to front left far"
 spoonAddFrontLeftFar
 putStrLn "\nsubtracting person from house"
 personSubhouse 
 putStrLn "\nrotating Spoon with fuzzy angle calculated from objA to objA"
 rotateSpoonWithB
 putStrLn "\nscale fuzzy spoon2 with scale factor from objA to big fuzzy ground template"
 scaleFuzzyVector   
 putStrLn "\ntransforamtions"

-- ** figures that show spatial reference frame transformations
calctransforms :: IO ()
calctransforms = do
 putStrLn "FIGURE A --------"
 pngfiga
-- putStrLn $ show  satfiga
 putStrLn "\n FIGURE B -----"
 pngfigb
-- putStrLn $ show  satfigb
 putStrLn "\n FIGURE C -----"
 pngfigc
-- putStrLn $ show  satfigc
 putStrLn "\n FIGURE D -----"
 figd
-- putStrLn $ show  satfigd
 putStrLn "\n FIGURE E -----"
 pngfige
-- putStrLn $ show  satfige 
 putStrLn "\n FIGURE F -----"
 figf1
-- putStrLn $ show satfigf
 print "DONE"


-- **  FIGURE  in paper
spoonAddFrontLeftFar :: IO ()
spoonAddFrontLeftFar  = do
 let figure1=  fuzzyAdd discreteSpace fuzzySpoon int 
     figure11 = fuzzyAdd discreteSpace fuzzySpoon fuzzyFrontLeft 
     int = (fuzzyIntersection fuzzyFrontLeft fuzzyFar )
 sequence_ [GnuPlot.makePngMap discreteSpace "fuzzy_spoon" fuzzySpoon
            ,GnuPlot.makePngMap discreteSpace "fuzzy_front_left" fuzzyFrontLeft
            ,GnuPlot.makePngMap discreteSpace "fuzzy_far" fuzzyFar
            ,GnuPlot.makePngMap discreteSpace "fuzzy_front_left_far" int
            ,GnuPlot.makePngMap discreteSpace "fuzzy_spoon_added_fuzzy_front_left_far" figure1
            ,GnuPlot.makePngMap discreteSpace "fuzzy_spoon_added_fuzzy_front_left" figure11] 

-- ** FIGURE  in paper
personSubhouse :: IO ()
personSubhouse = do
 let figure = fuzzySub discreteSpace person4quadrant house4quadrant 
 sequence_ [ GnuPlot.makePngMap discreteSpace "person4quadrant" person4quadrant
           , GnuPlot.makePngMap discreteSpace "house4quadrant" house4quadrant
           , GnuPlot.makePngMap discreteSpace "person4quadrant_sub_house4quadrant" figure]

-- ** FIGURE in paper rotation
rotateSpoonWithB :: IO ()
rotateSpoonWithB  = do
 let fuzzyAngle =fuzzyAngleofFuzzyVector discreteSpace objB 
     rotated = fuzzyRotation discreteSpace fuzzySpoon  fuzzyAngle
 sequence_ [GnuPlot.makePngMap discreteSpace "obj_B" objB
           ,GnuPlot.makePngMap discreteSpace "fuzzy_spoon" fuzzySpoon
           ,GnuPlot.makePngMap discreteSpace "fuzzy_spoon_rotated_by_fuzzy_angle_of_obj_B" rotated
           ]
 putStrLn "values for pgfplot polar for zero values"           
 getZeroProbAngles discreteSpace objB            
 putStrLn "values for pgfplot polar for NON zero values"           
 getNonZeroProbAngles  discreteSpace objB

-- ** crisp rotation
rotateCrisp :: IO ()
rotateCrisp = do
 let fuzzyAngle =fuzzyAngleofFuzzyVector discreteSpace objB 
     crispAngle = calcCentroidAngle discreteSpace fuzzyAngle
     rotated = crispRotation  fuzzySpoon crispAngle
 putStrLn $ "crispAngle:"++ show crispAngle
 sequence_ [GnuPlot.makePngMap discreteSpace "obj_B" objB
   ,GnuPlot.makePngMap discreteSpace "fuzzy_spoon" fuzzySpoon
   ,GnuPlot.makePngMap discreteSpace  ("fuzzy_spoon_rotated_by_crisp_angle_of_obj_B_"++ show crispAngle) rotated]

rotateCrisp2 :: IO ()
rotateCrisp2 = do
 let rotated = crispRotation  fuzzySpoon (45.0) 
 sequence_ [GnuPlot.makePngMap discreteSpace "fuzzy spoon rotated by crisp angle 45.0" rotated]
 
-- ** FIGURE in paper scaling

scaleFuzzyVector :: IO ()
scaleFuzzyVector = do
 let sf = calculateFuzzyScaleFactor discreteSpace centerobjA1 bigfuzzyGroundTemplate 
     sf' = calculateFuzzyScaleFactor' discreteSpace centerobjA1 bigfuzzyGroundTemplate 
     centerobjA1 = translateToCenterWithCentroid discreteSpace objA 
     crispScaleFactor = calcCentroidFuzzyScaleFactor discreteSpace sf
     scaled = fuzzyCentroidScale discreteSpace fuzzySpoon2  sf 
 putStrLn "scale factors for paper:"
 print  sf'    
 putStrLn $ "Crisp Scale Factor:"++ show  crispScaleFactor
 sequence_ [GnuPlot.makePngMap discreteSpace "obj_A" objA
           ,GnuPlot.makePngMap discreteSpace "centered_ obj_A" centerobjA1
           ,GnuPlot.makePngMap discreteSpace "bigfuzzyGroundTemplate_union_obj_A" $ fuzzyUnion objA bigfuzzyGroundTemplate
           ,GnuPlot.makePngMap discreteSpace "bigfuzzyGroundTempalte" bigfuzzyGroundTemplate  
           ,GnuPlot.makePngMap discreteSpace "fuzzy_spoon_2" fuzzySpoon2
           ,GnuPlot.makePngMap discreteSpace "scaled_spoon_2" scaled    ]

scaleFactor = convertScaleFactor4tikz $! calculateFuzzyScaleFactor' discreteSpace centerobjA1 bigfuzzyGroundTemplate 
  where centerobjA1 = translateToCenterWithCentroid discreteSpace objA 


-- * FIGURE in paper transformation
-- fuzzy vectors for reference frames:
refSpace =  discreteSpace

intrinsicRefHouse :: DiscreteSpace ->IO ()
intrinsicRefHouse space= do
 let  t =transform space fuzzyGroundTemplate house house houseFront person 
 putStrLn "intrinsicRefHouse"
 sequence_ [GnuPlot.makePngMap space "fuzzy_ground_template" fuzzyGroundTemplate  
                             ,GnuPlot.makePngMap space "house" house
                             ,GnuPlot.makePngMap space "house_front" houseFront
                             ,GnuPlot.makePngMap space "person" person
                             ] --,GnuPlot.makePngMap discreteSpace "intrinsic_reference_frame_house" t

-- * spatial reference transformations
-- ** figure (a) from paper
rotnorth = crispRotation north (-30.0) 

figa = fuzzyUnion north $ fuzzyUnion simon $ fuzzyUnion tree $ fuzzyUnion largehouse largehouseFront

figa1 =1 -- scaleAasBtoC 

pngfiga = GnuPlot.makePngMap refSpace "figure_a" figa
inta = GnuPlot.makeInteractiveMesh refSpace figa

satfiga= satisfies refSpace figa namedSpatialTemplates  0.8    

-- ** figure (b) from paper
fuzzyAngle = fuzzyAngleofFuzzyVector refSpace north 

ta = filter ((/=0.0).snd) $ calculateFuzzyAngle' refSpace north


rottree = fuzzyRotation refSpace tree  fuzzyAngle
rotsimon = simon -- crispTranslate rot (V.fromList [8.0,-8.0])
rothouse = fuzzyRotation refSpace largehouse  fuzzyAngle
rothouseFront =fuzzyRotation refSpace largehouseFront  fuzzyAngle

figb = fuzzyUnion rotsimon $ fuzzyUnion rottree $ fuzzyUnion rothouse rothouseFront

pngfigb =  GnuPlot.makePngMap refSpace "figure_b" figb
intb = GnuPlot.makeInteractiveMesh refSpace figb
inttree = GnuPlot.makeInteractiveMesh refSpace rottree
intthouse = GnuPlot.makeInteractiveMesh refSpace rothouse
intthouseFront = GnuPlot.makeInteractiveMesh refSpace rothouseFront
innt =  GnuPlot.makeInteractiveMesh refSpace $ fuzzyUnion largehouseFront largehouse
inttree1 = GnuPlot.makeInteractiveMesh refSpace tree

satfigb= satisfies refSpace (rottree) namedSpatialTemplates   0.8    

-- ground template muss nicht angepasse werden

-- ** figure (c) from paper

pngfigc :: IO ()
pngfigc = do
 let angle = fuzzyAngleofFuzzyVector discreteSpace north
     figure = fuzzyRotation discreteSpace subtraction angle
     subtraction = fuzzySub discreteSpace tree largehouse 
 sequence_ [ GnuPlot.makePngMap discreteSpace "tree" tree
           , GnuPlot.makePngMap discreteSpace "largehouse" largehouse
           , GnuPlot.makePngMap discreteSpace "fig_c" figure]


satfigc= satisfies refSpace transformedTree scalednamedRefFrames  0.8    
 where scalednamedRefFrames = map (\ (name,nframe)  ->(name, scaleAasBtoC refSpace nframe fuzzyGroundTemplate largehouse  )) namedSpatialTemplates
       angle = fuzzyAngleofFuzzyVector discreteSpace north
       transformedTree = fuzzyRotation discreteSpace subtraction angle
       subtraction = fuzzySub discreteSpace tree largehouse 

-- ** figure (d) from paper
figd = do
 let  frontdirection = fuzzySub refSpace largehouseFront largehouse
      angle = fuzzyAngleofFuzzyVector refSpace frontdirection 
      subtraction = fuzzySub refSpace tree largehouse 
      figure = fuzzyRotation refSpace subtraction  angle   
 sequence_ [GnuPlot.makePngMap discreteSpace "tree" tree
                 , GnuPlot.makePngMap discreteSpace "frontdirectin" frontdirection
                 , GnuPlot.makePngMap discreteSpace "subtraction" subtraction
                 , GnuPlot.makePngMap discreteSpace "largehouse" largehouse
                 , GnuPlot.makePngMap discreteSpace "largehouseFront" largehouseFront
                 , GnuPlot.makePngMap discreteSpace "fig_d" figure
                 ]

tbnn = GnuPlot.makeInteractiveMesh discreteSpace $ fuzzySub refSpace largehouseFront largehouse

tnm=  fuzzyRotation refSpace (fuzzySub refSpace tree largehouse  )  fuzzyAngleFront   
 where fuzzyFront = fuzzySub refSpace largehouseFront house 
       fuzzyAngleFront = fuzzyAngleofFuzzyVector refSpace fuzzyFront 

tbn = fitToVectorDomain $ C.crispVector 0.3 0.8
pngfigd= GnuPlot.makePngMap refSpace "figure_d" tnm

satfigd= satisfies refSpace tnm scalednamedRefFrames  0.8    
 where scalednamedRefFrames = map (\ (name,nframe)  ->(name, scaleAasBtoC refSpace nframe fuzzyGroundTemplate largehouse  )) namedSpatialTemplates 

-- ** figure (e) from paper
fige =  (fuzzySub refSpace tree largehouse  )

pngfige = GnuPlot.makePngMap refSpace "figure_e" $  fige

satfige= satisfies refSpace fige scalednamedRefFrames  0.8    
 where scalednamedRefFrames = map (\ (name,nframe)  ->(name, scaleAasBtoC refSpace nframe fuzzyGroundTemplate largehouse  )) namedSpatialTemplates 

-- ** figure (f) from paper

figf1 = do 
        let angle = fuzzyAngleofFuzzyVector refSpace frontdirection 
            frontdirection = fuzzySub refSpace simon largehouse
            subtraction = fuzzySub refSpace tree largehouse 
            figure = fuzzyRotation refSpace subtraction  angle   
        sequence_ [GnuPlot.makePngMap discreteSpace "tree" tree
                            , GnuPlot.makePngMap discreteSpace "largehouse" largehouse
                            , GnuPlot.makePngMap discreteSpace "largehouseFront" largehouseFront
                            , GnuPlot.makePngMap discreteSpace "fig_f" figure
                            ]

figf=fuzzyRotation refSpace(fuzzySub refSpace tree largehouse  )  fuzzyAngleObserver
  where fuzzyOrientationObserver = fuzzySub refSpace simon house 
        fuzzyAngleObserver = fuzzyAngleofFuzzyVector refSpace fuzzyOrientationObserver 

pngfigf = GnuPlot.makePngMap refSpace "figure_f" figf

satfigf= satisfies refSpace figf scalednamedRefFrames  0.8    
 where scalednamedRefFrames = map (\ (name,nframe)  ->(name, scaleAasBtoC refSpace nframe fuzzyGroundTemplate largehouse  )) namedSpatialTemplates


-- * other figures that are nice to see

-- | show all created Reference Frames
interReferenceFrame =GnuPlot.showFuzzyVectors discreteSpace  allSpatialTemplates


interactiveFuzzyVectors = GnuPlot.showFuzzyVectors discreteSpace fuzzyVectors
 where fuzzyVectors = [fuzzySpoon,fuzzyFork]

intSPatTemplate = GnuPlot.showFuzzyVectors discreteSpace allSpatialTemplates
exportAllFuzzyVectors= GnuPlot.exportFuzzyVectorstoPng discreteSpace names fuzzyVectors
 where names = ["Spoon","Fork","NearRight","SpoonUnionFork"]
       fuzzyVectors = [fuzzySpoon,fuzzyFork,fuzzyBackRight,fuzzySpoonUnionFork]
       fuzzySpoonUnionFork= fuzzyUnion fuzzySpoon fuzzyFork
