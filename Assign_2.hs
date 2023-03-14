{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2021
-}
module Assign_2 where
import Data.List (findIndex)
import GHC.OldList (elemIndex)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Michael Kim
-- Date: October 30th 2022
macid :: String
macid = "kim370"

type Vector4D = (Double,Double,Double,Double)



{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   This function gets the x value from the 4d vector
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     vs     | Vector4D input                               |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     x      | Double Output                                |
 - -----------------------------------------------------------------
 -}

getX :: Vector4D -> Double
getX (x,_,_,_) = x

{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
 -   This function gets the Y value from the 4d vector
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     vs     | Vector4D input                               |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     y      | Double Output                                |
 - -----------------------------------------------------------------
 -}

getY :: Vector4D -> Double
getY (_,y,_,_) = y


{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
 -   This function gets the Z value from the 4d vector
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     vs     | Vector4D input                               |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     z      | Double Output                                |
 - -----------------------------------------------------------------
 -}
getZ :: Vector4D -> Double
getZ (_,_,z,_) = z

{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   This function gets the m value from the 4d vector
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     vs     | Vector4D input                               |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     m      | Double Output                                |
 - -----------------------------------------------------------------
 -}
getM :: Vector4D -> Double
getM (_,_,_,m) = m

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
 -   This function multiplies each values from each vector with eachother
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     s      | Double input                                 |
 - |     vs     | Vector4D input                               |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     vs     | Vector4D Output                              |
 - -----------------------------------------------------------------
 -}
scalarMult :: Double -> Vector4D -> Vector4D
scalarMult s (x,y,z,m) = (x*s, y*s, z*s, m*s)

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
 -   This function adds each values from each vector with eachother
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     v0     | Vector4D input                                 |
 - |     v1     | Vector4D input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     vs     | Vector4D Output                                |
 - -----------------------------------------------------------------
 -}
add :: Vector4D -> Vector4D -> Vector4D
add (x0,y0,z0,m0) (x1,y1,z1,m1) = (x0+x1, y0+y1, z0+z1, m0+m1)


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
 -   This function multiplies each values from each vector with eachother, and then summed
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     v0     | Vector4D input                                 |
 - |     v1     | Vector4D input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     x      | Double Output                                |
 - -----------------------------------------------------------------
 -}
innerProduct :: Vector4D -> Vector4D -> Double
innerProduct (x0,y0,z0,m0) (x1,y1,z1,m1) = x0*x1 + y0*y1 + z0*z1 + m0*m1

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
 -   This function finds the distance between two vectors
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     v0     | Vector4D input                                 |
 - |     v1     | Vector4D input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     x      | Double Output                                |
 - -----------------------------------------------------------------
 -}
distance :: Vector4D -> Vector4D -> Double
distance (x0,y0,z0,m0) (x1,y1,z1,m1) = sqrt ((x0-x1)**2 + (y0-y1)**2 + (z0-z1)**2 + (m0-m1)**2)

{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
- Description:
 -   This function finds the distance between two vectors
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     vs     | Vector4D list input                          |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     Vector4D      | Double Output                         |
 - -----------------------------------------------------------------
 -}

maxDistance :: [Vector4D] -> Vector4D
maxDistance [] = (0,0,0,0)
maxDistance vs = checkDist vs maxDist
            where
              maxDist =  maximum (foldr (\x -> (++) [distance x (0, 0, 0, 0)]) [] vs)

--Can be done with just one function, but too inefficient. Calcuations are being done multiple times
checkDist :: [Vector4D] -> Double -> Vector4D
checkDist [] _ = (0,0,0,0)
checkDist vs maxDist 
              | currDist == maxDist = head vs
              | otherwise = checkDist (tail vs) maxDist
              where
                currDist = distance (head vs) (0,0,0,0)


{-
Function: scalarMult
Test Case Number: 1
Input: 5.0 (3.0, 6.0, 8.0, 2.0)
Expected Output: (15.0, 30.0, 40.0, 10.0)
Actual Output: (15.0, 30.0, 40.0, 10.0)

Function: scalarMult
Test Case Number: 2
Input: 8.0 (12.0, 64.0, 23.0, 0.0)
Expected Output: (96.0, 512.0, 184.0,0.0)
Actual Output: (96.0, 512.0, 184.0,0.0)

Function: scalarMult
Test Case Number: 3
Input: 0.0 (52.0, 23.0, 96.0, 1.0)
Expected Output: (0.0, 0.0, 0.0, 0.0)
Actual Output: (0.0, 0.0, 0.0, 0.0)



Function: add
Test Case Number: 1
Input: (23.0, 43.0, 2.0, 0.0) (3.0, 3.0, 1.0, 6.0) 
Expected Output: (26.0, 46.0, 3.0, 6.0) 
Actual Output: (26.0, 46.0, 3.0, 6.0) 

Function: add
Test Case Number: 2
Input: (27.0, 6.0, 89.0, 6.0) (36.0, 0.0, 0.0, 7.0) 
Expected Output: (63.0, 6.0, 89.0, 13.0) 
Actual Output: (63.0, 6.0, 89.0, 13.0) 

Function: add
Test Case Number: 3
Input: (5.0, 43.0, 4.0, 67.0) (56.0, 27.0, 645.0, 109.0) 
Expected Output: (61.0, 70.0, 649.0, 176.0) 
Actual Output: (61.0, 70.0, 649.0, 176.0) 



Function: innerProduct
Test Case Number: 1
Input: (43.0, 23.0, 69.0, 1.0) (1.0, 0.0, 9.0, 16.0) 
Expected Output: 680.0
Actual Output: 680.0

Function: innerProduct
Test Case Number: 2
Input: (65.0, 3.0, 238.0, 1.0) (142.0, 6.0, 38.0, 13.0) 
Expected Output: 18305.0
Actual Output: 18305.0

Function: innerProduct
Test Case Number: 3
Input: (3.0, 242.0, 35.0, 12.0) (65.0, 53.0, 75.0, 10.0) 
Expected Output: 15766.0
Actual Output: 15766.0


NOTE: reason why expected output is as long as it is because I used an online calculator

Function: distance
Test Case Number: 1
Input: (46.0, 452.0, 48.0, 82.0) (5.0, 7.0, 13.0, 10.0) 
Expected Output: 453.999
Actual Output: 453.9988986770783

Function: distance
Test Case Number: 2
Input: (44.0, 446.0, 89.0, 8.0) (82.0, 63.0, 567.0, 10.0) 
Expected Output: 613.695
Actual Output: 613.694549429926

Function: distance
Test Case Number: 3
Input: (11.0, 78.0, 56.0, 789.0) (359.0, 76.0, 27.0, 93.0) 
Expected Output: 778.694
Actual Output: 778.6944201675007



Function: maxDistance
Test Case Number: 1
Input: [(54.0, 84.0, 545.0, 9.0), (373.0, 19.0, 209.0, 57.0), (45.0, 34.0, 6.0, 72.0)] 
Expected Output: (54.0, 84.0, 545.0, 9.0),
Actual Output: (54.0, 84.0, 545.0, 9.0),

Function: maxDistance
Test Case Number: 2
Input: [(67.0, 48.0, 675.0, 34.0)]
Expected Output: (67.0, 48.0, 675.0, 34.0)
Actual Output: (67.0, 48.0, 675.0, 34.0)

Function: maxDistance
Test Case Number: 3
Input: [(20.0, 503.0, 97.0, 21.0), (73.0, 54.0, 93.0, 33.0)] 
Expected Output: (20.0,503.0,97.0,21.0)
Actual Output: (20.0,503.0,97.0,21.0)
-}