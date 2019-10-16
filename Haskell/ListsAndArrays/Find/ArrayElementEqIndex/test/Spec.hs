{-# LANGUAGE RecordWildCards #-}

import Data.Array          (Array, listArray) 
import Data.Foldable       (for_)   
import Test.Hspec          (Spec, describe, it, shouldBe)
import Test.Hspec.Runner   (configFastFail, defaultConfig, hspecWith)

import ArrayElementEqIndex (arrayElementEqIndex)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = 
    for_ cases test
    where
        test Case{..} = it description assertion
            where 
            assertion = arrayElementEqIndex input `shouldBe` expected

data Case = Case { description  :: String
                 , input        :: Array Integer Integer
                 , expected     :: Bool 
                 }

cases :: [Case]
cases = [ Case { description = "one element larger than 1"
               , input       = listArray (1,1) [10]
               , expected    = False
               }
        , Case { description = "one element eq 1"
               , input       = listArray (1,1) [1]
               , expected    = True
               }
        , Case { description = "one element less than 1"
               , input       = listArray (1, 1) [0]
               , expected    = False
               }
        , Case { description = "two elements both larger than i's"
               , input       = listArray (1,2) [9, 10]
               , expected    = False
               }
        , Case { description = "two elements both smaller than i's"
               , input       = listArray (1,2) [-5, 0]
               , expected    = False
               }
        , Case { description = "two elements one eq i"
               , input       = listArray (1,2) [-5, 2]
               , expected    = True
               }
        , Case { description = "two elements both eq i"
               , input       = listArray (1,2) [1, 2]
               , expected    = True
               }
        , Case { description = "three elements none eq i"
               , input       = listArray (1,3) [-1, 0, 2]
               , expected    = False
               }
        , Case { description = "three elements one eq i"
               , input       = listArray (1,3) [-1, 0, 3]
               , expected    = True
               }
        , Case { description = "three elements two eq i"
               , input       = listArray (1,3) [0, 2, 3]
               , expected    = True
               }
        , Case { description = "three elements three eq i"
               , input       = listArray (1,3) [1, 2, 3]
               , expected    = True
               }
        , Case { description = "long array none eq i"
               , input       = listArray (1,10) [2..11]
               , expected    = False
               }
        , Case { description = "long array none eq i"
               , input       = listArray (1,10) [2..11]
               , expected    = False
               }
        , Case { description = "long array all eq i"
               , input       = listArray (1,10) [1..10]
               , expected    = True
               }
        , Case { description = "long array all neg none eq i"
               , input       = listArray (1,10) [(-10)..(-1)]
               , expected    = False
               }
        , Case { description = "large numbers one eq"
               , input       = listArray (1,10) [1, 3, 4, 5, 19, 56, 2345, 253232, 523232, 35231232]
               , expected    = False
               }
        , Case { description = "mostly neg numbers one pos one eq"
               , input       = listArray (1,10) $ [(-10)..(-2)]++[10]
               , expected    = True
               }              ]
