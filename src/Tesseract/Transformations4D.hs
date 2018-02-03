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
