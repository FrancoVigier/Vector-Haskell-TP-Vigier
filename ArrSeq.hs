module ArrSeq where

import Seq
import Par
import qualified Arr as A


contraccionAux :: (a -> a -> a) -> A.Arr a -> Int -> a
contraccionAux f s i | (lengthS s) - i == 0 = undefined
                     | (lengthS s) - i == 1 = let x = nthS s i
                                              in x
                     | otherwise            = let 
                                                 x = nthS s i 
                                                 y = nthS s (i+1)
                                              in f x y

contraccion :: (a -> a -> a) -> A.Arr a -> A.Arr a
contraccion f s = let
                     l = lengthS s
                     rlen =  if even l then (div l 2) else ((div l 2) +1)
                  in tabulateS (\i -> contraccionAux f s (i*2)) rlen


instance Seq A.Arr where
    emptyS = A.empty

    singletonS x = tabulateS (\_ -> x) 1

    lengthS = (A.length)

    nthS = (A.!)

    tabulateS = (A.tabulate)

    mapS f s = tabulateS (\i -> f (nthS s i)) (lengthS s)

    filterS f s = A.flatten (mapS (\x -> if f x then (singletonS x) else emptyS) s)

    appendS sa sb = A.flatten (fromList [sa, sb])
        
    takeS s n = A.subArray 0 n s

    dropS s n = A.subArray (n) ((lengthS s) - n) s

    showtS s | lengthS s == 0 = EMPTY
             | lengthS s == 1 = ELT (nthS s 0)
             | otherwise      = let
                                    half = div (lengthS s) 2 
                                    (l, r) = (takeS s half) ||| (dropS s half)
                                in NODE l r

    showlS s | lengthS s == 0 = NIL
             | otherwise      = CONS (nthS s 0) (dropS s 1)
    
    joinS = (A.flatten)
    
    reduceS f b s | lengthS s == 0 = b
                  | lengthS s == 1 = f b (nthS s 0)
                  | otherwise      = reduceS f b (contraccion f s) -- se asume f \in O(1)
                       
    scanS f b s | lengthS s == 0 = (emptyS, b)
                | lengthS s == 1 = let x = nthS s 0 
                                   in (singletonS b, f b x)                      
                | otherwise      = let 
                                        l = lengthS s
                                        contracted = (contraccion f s)
                                        (s', last) = scanS f b contracted
                                        
                                        r i | even i    = nthS s' (div i 2)
                                            | otherwise = let 
                                                            a = nthS s' (div i 2)
                                                            b = nthS s (i-1)
                                                         in f a b -- se asume f \in O(1)
                                              
                                   in (tabulateS (r) l, last)

                   
    fromList = (A.fromList)




