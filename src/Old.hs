module Old where

myLambda :: FuzzyVector-> FuzzyVector-> DiscreteSpace -> V.Vector Float -> FuzzyScaleFactor
myLambda myA myB space v2 scalefactor = maximum' .map (\v1 -> min (myA v1) (myB v2)) $ matchedVectors
 where matchedVectors = filter ((==v2) . roundVector 2 . (crispScale scalefactor) ) $ createDescriteVectorsForSpace space


-- | nub because many vectors have the same norm vector, e.g. [1.0,1.0], [2.0,2.0] = [0.707...,0.707..]
createDescriteUnitVectorForSpace :: DiscreteSpace -> [V.Vector Float]
createDescriteUnitVectorForSpace  space= nub . map (normVector) $ createDescriteVectorsForSpace space


normVector :: V.Vector Float ->V.Vector Float
normVector v = V.map (/ norm) v
 where norm =  N.pnorm N.PNorm2 v


p1= (2.0,1.8,0.0)
p2= (2.0,4.2,0.0)
p3= (4.0,4.2,0.0)
p4= (4.0,1.8,0.0)

p5 = (2.6,2.7,1.8)
p6= (2.6,3.8,1.8)
p7 = (3.4,3.8,1.8)
p8= (3.4,2.7,1.8)

p1p = cartesian3DToPolar3D p1
p2p = cartesian3DToPolar3D p2
p3p = cartesian3DToPolar3D p3
p4p = cartesian3DToPolar3D p4

p5p = cartesian3DToPolar3D p5
p6p = cartesian3DToPolar3D p6
p7p = cartesian3DToPolar3D p7
p8p = cartesian3DToPolar3D p8

radialDistance3D (x,y,z) = sqrt (x*x + y*y + z*z)

cartesian3DToPolar3D (x,y,z) = (r,alpha*180/pi, beta*180/pi)
                                where r     = radialDistance3D (x,y,z) 
                                      alpha = acos(z/r)
                                      beta  = atan2 y x
