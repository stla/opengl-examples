module TruncatedTesseract.Data
  where

ttesseractVertices :: [[Double]]
ttesseractVertices =
  let x = 1 + sqrt 2 in
  map (map (/ sqrt(1 + 3*x*x)))
    [ [ -1.0, -x, -x, -x ]
    , [ -1.0, -x, -x, x ]
    , [ -1.0, -x, x, -x ]
    , [ -1.0, -x, x, x ]
    , [ -1.0, x, -x, -x ]
    , [ -1.0, x, -x, x ]
    , [ -1.0
      , x
      , x
      , -x
      ]
    , [ -1.0
      , x
      , x
      , x
      ]
    , [ 1.0
      , -x
      , -x
      , -x
      ]
    , [ 1.0
      , -x
      , -x
      , x
      ]
    , [ 1.0
      , -x
      , x
      , -x
      ]
    , [ 1.0
      , -x
      , x
      , x
      ]
    , [ 1.0
      , x
      , -x
      , -x
      ]
    , [ 1.0
      , x
      , -x
      , x
      ]
    , [ 1.0
      , x
      , x
      , -x
      ]
    , [ 1.0
      , x
      , x
      , x
      ]
    , [ -x
      , -1.0
      , -x
      , -x
      ]
    , [ -x
      , -1.0
      , -x
      , x
      ]
    , [ -x
      , -1.0
      , x
      , -x
      ]
    , [ -x
      , -1.0
      , x
      , x
      ]
    , [ -x
      , 1.0
      , -x
      , -x
      ]
    , [ -x
      , 1.0
      , -x
      , x
      ]
    , [ -x
      , 1.0
      , x
      , -x
      ]
    , [ -x
      , 1.0
      , x
      , x
      ]
    , [ x
      , -1.0
      , -x
      , -x
      ]
    , [ x
      , -1.0
      , -x
      , x
      ]
    , [ x
      , -1.0
      , x
      , -x
      ]
    , [ x
      , -1.0
      , x
      , x
      ]
    , [ x
      , 1.0
      , -x
      , -x
      ]
    , [ x
      , 1.0
      , -x
      , x
      ]
    , [ x
      , 1.0
      , x
      , -x
      ]
    , [ x
      , 1.0
      , x
      , x
      ]
    , [ -x
      , -x
      , -1.0
      , -x
      ]
    , [ -x
      , -x
      , -1.0
      , x
      ]
    , [ -x
      , -x
      , 1.0
      , -x
      ]
    , [ -x
      , -x
      , 1.0
      , x
      ]
    , [ -x
      , x
      , -1.0
      , -x
      ]
    , [ -x
      , x
      , -1.0
      , x
      ]
    , [ -x
      , x
      , 1.0
      , -x
      ]
    , [ -x
      , x
      , 1.0
      , x
      ]
    , [ x
      , -x
      , -1.0
      , -x
      ]
    , [ x
      , -x
      , -1.0
      , x
      ]
    , [ x
      , -x
      , 1.0
      , -x
      ]
    , [ x
      , -x
      , 1.0
      , x
      ]
    , [ x
      , x
      , -1.0
      , -x
      ]
    , [ x
      , x
      , -1.0
      , x
      ]
    , [ x
      , x
      , 1.0
      , -x
      ]
    , [ x
      , x
      , 1.0
      , x
      ]
    , [ -x
      , -x
      , -x
      , -1.0
      ]
    , [ -x
      , -x
      , -x
      , 1.0
      ]
    , [ -x
      , -x
      , x
      , -1.0
      ]
    , [ -x
      , -x
      , x
      , 1.0
      ]
    , [ -x
      , x
      , -x
      , -1.0
      ]
    , [ -x
      , x
      , -x
      , 1.0
      ]
    , [ -x
      , x
      , x
      , -1.0
      ]
    , [ -x
      , x
      , x
      , 1.0
      ]
    , [ x
      , -x
      , -x
      , -1.0
      ]
    , [ x
      , -x
      , -x
      , 1.0
      ]
    , [ x
      , -x
      , x
      , -1.0
      ]
    , [ x
      , -x
      , x
      , 1.0
      ]
    , [ x
      , x
      , -x
      , -1.0
      ]
    , [ x, x, -x, 1.0 ]
    , [ x, x, x, -1.0 ]
    , [ x, x, x, 1.0 ]
    ]

ttesseractEdges :: [(Int, Int)]
ttesseractEdges = [ ( 0 , 8 )
                  , ( 0 , 16 )
                  , ( 0 , 32 )
                  , ( 0 , 48 )
                  , ( 1 , 9 )
                  , ( 1 , 17 )
                  , ( 1 , 33 )
                  , ( 1 , 49 )
                  , ( 2 , 10 )
                  , ( 2 , 18 )
                  , ( 2 , 34 )
                  , ( 2 , 50 )
                  , ( 3 , 11 )
                  , ( 3 , 19 )
                  , ( 3 , 35 )
                  , ( 3 , 51 )
                  , ( 4 , 12 )
                  , ( 4 , 20 )
                  , ( 4 , 36 )
                  , ( 4 , 52 )
                  , ( 5 , 13 )
                  , ( 5 , 21 )
                  , ( 5 , 37 )
                  , ( 5 , 53 )
                  , ( 6 , 14 )
                  , ( 6 , 22 )
                  , ( 6 , 38 )
                  , ( 6 , 54 )
                  , ( 7 , 15 )
                  , ( 7 , 23 )
                  , ( 7 , 39 )
                  , ( 7 , 55 )
                  , ( 8 , 24 )
                  , ( 8 , 40 )
                  , ( 8 , 56 )
                  , ( 9 , 25 )
                  , ( 9 , 41 )
                  , ( 9 , 57 )
                  , ( 10 , 26 )
                  , ( 10 , 42 )
                  , ( 10 , 58 )
                  , ( 11 , 27 )
                  , ( 11 , 43 )
                  , ( 11 , 59 )
                  , ( 12 , 28 )
                  , ( 12 , 44 )
                  , ( 12 , 60 )
                  , ( 13 , 29 )
                  , ( 13 , 45 )
                  , ( 13 , 61 )
                  , ( 14 , 30 )
                  , ( 14 , 46 )
                  , ( 14 , 62 )
                  , ( 15 , 31 )
                  , ( 15 , 47 )
                  , ( 15 , 63 )
                  , ( 16 , 20 )
                  , ( 16 , 32 )
                  , ( 16 , 48 )
                  , ( 17 , 21 )
                  , ( 17 , 33 )
                  , ( 17 , 49 )
                  , ( 18 , 22 )
                  , ( 18 , 34 )
                  , ( 18 , 50 )
                  , ( 19 , 23 )
                  , ( 19 , 35 )
                  , ( 19 , 51 )
                  , ( 20 , 36 )
                  , ( 20 , 52 )
                  , ( 21 , 37 )
                  , ( 21 , 53 )
                  , ( 22 , 38 )
                  , ( 22 , 54 )
                  , ( 23 , 39 )
                  , ( 23 , 55 )
                  , ( 24 , 28 )
                  , ( 24 , 40 )
                  , ( 24 , 56 )
                  , ( 25 , 29 )
                  , ( 25 , 41 )
                  , ( 25 , 57 )
                  , ( 26 , 30 )
                  , ( 26 , 42 )
                  , ( 26 , 58 )
                  , ( 27 , 31 )
                  , ( 27 , 43 )
                  , ( 27 , 59 )
                  , ( 28 , 44 )
                  , ( 28 , 60 )
                  , ( 29 , 45 )
                  , ( 29 , 61 )
                  , ( 30 , 46 )
                  , ( 30 , 62 )
                  , ( 31 , 47 )
                  , ( 31 , 63 )
                  , ( 32 , 34 )
                  , ( 32 , 48 )
                  , ( 33 , 35 )
                  , ( 33 , 49 )
                  , ( 34 , 50 )
                  , ( 35 , 51 )
                  , ( 36 , 38 )
                  , ( 36 , 52 )
                  , ( 37 , 39 )
                  , ( 37 , 53 )
                  , ( 38 , 54 )
                  , ( 39 , 55 )
                  , ( 40 , 42 )
                  , ( 40 , 56 )
                  , ( 41 , 43 )
                  , ( 41 , 57 )
                  , ( 42 , 58 )
                  , ( 43 , 59 )
                  , ( 44 , 46 )
                  , ( 44 , 60 )
                  , ( 45 , 47 )
                  , ( 45 , 61 )
                  , ( 46 , 62 )
                  , ( 47 , 63 )
                  , ( 48 , 49 )
                  , ( 50 , 51 )
                  , ( 52 , 53 )
                  , ( 54 , 55 )
                  , ( 56 , 57 )
                  , ( 58 , 59 )
                  , ( 60 , 61 )
                  , ( 62 , 63 )
                  ]

ttesseractFacet :: [[Int]] -- pas utilisé
ttesseractFacet = [ [ 27 , 43 , 59 ]
                  , [ 28 , 44 , 60 ]
                  , [ 24 , 40 , 56 ]
                  , [ 25 , 41 , 57 ]
                  , [ 31 , 47 , 63 ]
                  , [ 29 , 45 , 61 ]
                  , [ 30 , 46 , 62 ]
                  , [ 26 , 42 , 58 ]
                  , [ 24 , 28 , 44 , 46 , 30 , 26 , 42 , 40 ]
                  , [ 40 , 42 , 58 , 59 , 43 , 41 , 57 , 56 ]
                  , [ 25 , 29 , 45 , 47 , 31 , 27 , 43 , 41 ]
                  , [ 24 , 28 , 60 , 61 , 29 , 25 , 57 , 56 ]
                  , [ 26 , 30 , 62 , 63 , 31 , 27 , 59 , 58 ]
                  , [ 44 , 46 , 62 , 63 , 47 , 45 , 61 , 60 ]
                  ]

tetrahedralFacets :: [[Int]]
tetrahedralFacets = [ [ 0 , 16 , 32 , 48 ]
                    , [ 11 , 27 , 43 , 59 ]
                    , [ 12 , 28 , 44 , 60 ]
                    , [ 8 , 24 , 40 , 56 ]
                    , [ 9 , 25 , 41 , 57 ]
                    , [ 15 , 31 , 47 , 63 ]
                    , [ 13 , 29 , 45 , 61 ]
                    , [ 14 , 30 , 46 , 62 ]
                    , [ 10 , 26 , 42 , 58 ]
                    , [ 3 , 19 , 35 , 51 ]
                    , [ 2 , 18 , 34 , 50 ]
                    , [ 1 , 17 , 33 , 49 ]
                    , [ 4 , 20 , 36 , 52 ]
                    , [ 5 , 21 , 37 , 53 ]
                    , [ 6 , 22 , 38 , 54 ]
                    , [ 7 , 23 , 39 , 55 ]
                    ]
