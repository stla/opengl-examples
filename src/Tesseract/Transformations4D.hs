module Tesseract.Transformations4D
  where

project4D :: [Double] -> [Double]
project4D x = map (/(1-x!!3)) [2 * x!!0, 2 * x!!1, 2 * x!!2]

rotate4D :: Double -> Double -> Double -> [Double] -> [Double]
rotate4D theta phi alpha x =
  [ a*p - b*q - c*r - d*s
  , a*q + b*p + c*s - d*r
  , a*r - b*s + c*p + d*q
  , a*s + b*r - c*q + d*p ]
  where
    a = cos alpha
    b = sintheta * cos phi * sinalpha
    sintheta = sin theta
    sinalpha = sin alpha
    c = sintheta * sin phi * sinalpha
    d = cos theta * sinalpha
    p = x!!0
    q = x!!1
    r = x!!2
    s = x!!3

leftIsoclinic :: Double -> Double -> Double -> [Double] -> [Double]
leftIsoclinic theta phi alpha x =
  [ q0*x0 - q1*x1 - q2*x2 - q3*x3
  , q1*x0 + q0*x1 - q3*x2 + q2*x3
  , q2*x0 + q3*x1 + q0*x2 - q1*x3
  , q3*x0 - q2*x1 + q1*x2 + q0*x3 ]
  where
    x0 = x!!0
    x1 = x!!1
    x2 = x!!2
    x3 = x!!3
    q0 = cos alpha
    q1 = sin theta * cos phi * sin alpha
    q2 = sin theta * sin phi * sin alpha
    q3 = cos theta * sin alpha

rightIsoclinic :: Double -> Double -> Double -> [Double] -> [Double]
rightIsoclinic theta phi alpha x =
  [ q0*x0 - q1*x1 - q2*x2 - q3*x3
  , q1*x0 + q0*x1 + q3*x2 - q2*x3
  , q2*x0 - q3*x1 + q0*x2 + q1*x3
  , q3*x0 + q2*x1 - q1*x2 + q0*x3 ]
  where
    x0 = x!!0
    x1 = x!!1
    x2 = x!!2
    x3 = x!!3
    q0 = cos alpha
    q1 = sin theta * cos phi * sin alpha
    q2 = sin theta * sin phi * sin alpha
    q3 = cos theta * sin alpha

-- simpleRotation :: Double -> Double -> Double -> [Double] -> [Double]
-- simpleRotation theta phi alpha x =
--   rightIsoclinic theta phi (-alpha) (leftIsoclinic theta phi alpha x)

simpleRotation :: Double -> [Double] -> [Double]
simpleRotation theta x =
  [ x0
  , x1
  , cos theta * x2 - sin theta * x3
  , sin theta * x2 + cos theta * x3 ]
  where
    x0 = x!!0
    x1 = x!!1
    x2 = x!!2
    x3 = x!!3
