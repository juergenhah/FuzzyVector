module Distributions where
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra  as N

type Probability = Float
type  Distribution= [Probability]

data DiscreteSpace =S {minX:: Float
                          , minY::Float
                          , maxX:: Float
                          , maxY::Float
                          , stepsize::Float}

-- * distributions
subFork :: V.Vector Float -> Probability
subFork v
 | x < 0 || x> 1 =0
 | y < 2 || y> 3 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v


subSpoon ::  V.Vector Float -> Probability
subSpoon v
 | x < 3 || x> 4 =0
 | y < 3 || y> 4 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v



newFork :: V.Vector Float -> Probability
newFork v
 | x < 0 || x> 1 =0
 | y < 3 || y> 4 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v


myFork :: V.Vector Float -> Probability
myFork v
 | x < 0 || x> 1 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

toRotFork :: V.Vector Float -> Probability
toRotFork v
 | x < 0 || x> 1 =0
 | y < 0 || y> 2 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v
       

toRotSpoon :: V.Vector Float -> Probability
toRotSpoon v
 | x < 0 || x> 2 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v
       
normSpoon :: V.Vector Float -> Probability
normSpoon v
 | x < 4 || x> 5 =0
 | y < 0 || y> 1 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

mySpoon ::  V.Vector Float -> Probability
mySpoon v
 | x < 2 || x> 3 =0
 | y < 2 || y> 3 =0
 | otherwise = 1
 where x = V.head v
       y = V.head.V.drop 1 $ v

myPyramide :: V.Vector Float -> Probability
myPyramide v
 | x<0 || y<0 = 0
 | x>=5 || y>=5 = 0
 | (0<= x && x<= 1) && (0<=y && y<= 1)=1
 | otherwise = (-1/4) * sqrt(x*x+y*y) +6/4
   where x = V.head v
         y = V.head.V.drop 1 $ v

mySteps :: V.Vector Float -> Probability
mySteps v
 | (x >= (2+xdel) && x<=(3+xdel)) && (y >= (2+ydel) && y<=(3+ydel))=1
 | (x >= (1+xdel) && x<=(4+xdel)) && (y >= (1+ydel) && y<=(4+ydel))=0.66
 | (x >= (0+xdel) && x<=(5+xdel)) && (y >= (0+ydel) && y<=(5+ydel))=0.33
 | otherwise = 0
    where x = V.head v
          y = V.head.V.drop 1 $ v
          xdel= 2
          ydel= 1

myXAxis :: V.Vector Float -> Probability
myXAxis v
 | x  <3 || x>4 =0
 | y  <0 || y>1 =0
 |  otherwise =1
 where x = V.head v
       y = V.head.V.drop 1 $ v

myYAxis :: V.Vector Float -> Probability
myYAxis v
 | x  <0 || x>1 =0
 | y  <7 || y>8 =0
 | otherwise =1
 where x = V.head v
       y = V.head.V.drop 1 $ v
