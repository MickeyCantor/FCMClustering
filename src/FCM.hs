module FCM where

import Control.Applicative
import Data.List

points = [[1,2],[3,4],[5,6]] :: [[Double]]
centers = [[2,2], [4,4]] :: [[Double]]

calcAllMembership :: [[Double]] -> [[Double]] -> Double -> [[Double]]

calcAllMembership x c m
  |null x               = []
  |otherwise            = membership : calcAllMembership (tail x) c m
  where
    membership = foldr (\center list -> calcMembership (head x) center c m : list) [] c

calcMembership :: [Double] -> [Double] -> [[Double]] -> Double -> Double

calcMembership x c cx m = 1.0 / sum normsSquared where
  normsSquared = (**) <$> norms <*> ZipList (repeat (2.0 / (m-1.0)))
  norms = (/) <$> ZipList (repeat (getNormDiff x c)) <*> ZipList bottom
  bottom = map (getNormDiff x) cx


getNormDiff :: [Double] -> [Double] -> Double

getNormDiff xs ys = sqrt $  foldl (+) 0.0 (getZipList list) where
  list = (**) <$> diffList <*> ZipList (repeat 2.0)
  diffList = (-) <$> ZipList xs <*> ZipList ys

recalcCenter :: [[Double]] -> [[Double]] -> Double -> [[Double]]

recalcCenter x u m
  |null $ head u          = []
  |otherwise              = newc : recalcCenter x u1 m
  where
    newc      = getZipList $ (/) <$> ZipList sumux <*> (ZipList . repeat . sum) (map (**m) u0)
    sumux     = foldl (\x i -> getZipList $ (+) <$> ZipList x <*> ZipList i) [0.0,0.0] ux
    ux        = getZipList $ (\x y -> map ((x ** m) *) y) <$> ZipList u0 <*> ZipList x
    u0        = foldr (\x xs -> head x : xs) [] u
    u1        = foldr (\x xs -> tail x : xs) [] u

--Transpose the fucking things before they go in here
getMatrixNormDiff :: [[Double]] -> [[Double]] -> Double

getMatrixNormDiff xs ys = sqrt $ sum list where
  list = getZipList $ (**) <$> ZipList catDiff <*> ZipList (repeat 2)
  catDiff = concat $ getMatrixDiff xs ys

getMatrixDiff :: [[Double]] -> [[Double]] ->[[Double]]

getMatrixDiff x y
  |null x     = []
  |otherwise  = getZipList list : getMatrixDiff (tail x) (tail y)
  where list = (-) <$> ZipList (head x) <*> ZipList (head y)


fcmCluster :: [[Double]] -> [[Double]] -> Double -> Double -> [[Double]]

fcmCluster x c m eps
  |getMatrixNormDiff (transpose u1) (transpose u0) < eps  = u1
  |otherwise                                              = fcmCluster x c1 m eps
  where
    u0 = calcAllMembership x c m
    u1 = calcAllMembership x c1 m
    c1 = recalcCenter x u0 m
