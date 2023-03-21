import Control.Exception
import Seq
import ArrSeq
import ListSeq

import qualified Arr as A

test13 = let t = (tabulateS (\i -> 1) 8)::A.Arr Int
             x = reduceS (+) 1 t
             t1 = assert (x == 9) True
         in t1

test14 = let t = (tabulateS (\i -> 1) 6)::A.Arr Int
             x = reduceS (-) 0 t
             t1 = assert (x == 0) True
         in t1
        
test15 = let t = (fromList [])::A.Arr Int
             x = reduceS (-) 5 t
             t1 = assert (x == 5) True
         in t1

test16 = let t = (tabulateS (\i -> 1) 8)::A.Arr Int
             (x, l) = scanS (+) 1 t
             t1 = assert (l == 9) True
             t2 = assert (lengthS x == 8) True
         in t1 && t2
         
         
test17 = let t = (fromList [])::A.Arr Int
             (x, l) = scanS (+) 5 t
             t1 = assert (l == 5) True
             t2 = assert (lengthS x == 0) True
         in t1 && t2
         
testArr = test13 && test14 && test15 && test16 && test17
          
ltest13 = let t = (tabulateS (\i -> 1) 8)::[] Int
              x = reduceS (+) 1 t
              t1 = assert (x == 9) True
          in t1


ltest14 = let t = (tabulateS (\i -> 1) 6)::[] Int
              x = reduceS (-) 0 t
              t1 = assert (x == 0) True
          in t1

ltest15 = let t = ([])::[] Int
              x = reduceS (-) 5 t
              t1 = assert (x == 5) True
          in t1

ltest16 = let t = (tabulateS (\i -> 1) 8)::[] Int
              (x, l) = scanS (+) 1 t
              t1 = assert (l == 9) True
              t2 = assert (lengthS x == 8) True
          in t1 && t2
          
ltest17 = let t = ([])::[] Int
              (x, l) = scanS (+) 5 t
              t1 = assert (l == 5) True
              t2 = assert (lengthS x == 0) True
          in t1 && t2
         
testList = ltest13 && ltest14 && ltest15 && ltest16 && ltest17


main = print (testList && testArr)
