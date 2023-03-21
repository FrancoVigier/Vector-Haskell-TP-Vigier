module ListSeq where

import Seq
import Par

contraccion :: (a -> a -> a) -> [] a -> [] a
contraccion f [] = []
contraccion f [x] = [x]
contraccion f (x:y:xs) = let (v, vs) = (f x y) ||| (contraccion f xs)
                         in v:vs

expandir :: (a -> a -> a) -> [] a -> [] a -> Int -> [] a
expandir f [] []  i                             = []
expandir f s@(x:y:xs) s'@(x':xs') i | even i    = x':(expandir f s s' (i+1))
                                    | otherwise = let (y, ys) = (f x x') ||| (expandir f xs xs' (i+1))
                                                  in y:ys


instance Seq [] where
    emptyS = []

    singletonS x = [x]

    lengthS = (length)

    nthS = (!!)

    tabulateS f n = map f [0..(n-1)]
    
    mapS f [] = []
    mapS f (x:xs) = let (r, rs) = (f x) ||| mapS f xs -- aca f puede no ser barata
                    in r:rs

    filterS = (filter)

    appendS = (++) 
        
    takeS xs i = take i xs

    dropS xs i = drop i xs

    showtS [] = EMPTY
    showtS [x] = ELT x
    showtS xs = let 
                    half = div (lengthS xs) 2 
                    (l, r) = (takeS xs half) ||| (dropS xs half)
                in NODE l r

    showlS [] = NIL
    showlS (x:xs) = CONS x xs
    
    joinS (x:[]) = x
    joinS (x:y:[]) = appendS x y
    joinS (xs) = jointS (showtS xs)
                    where jointS EMPTY = []
                          jointS (ELT x) = joinS [x]
                          jointS (NODE l r) = appendS (joinS l) (joinS r)

    reduceS f b s | lengthS s == 0 = b
                  | lengthS s == 1 = let 
                                        x = nthS s 0 
                                     in f b x
                  | otherwise      = reduceS f b (contraccion f s)

    scanS f b s | lengthS s == 0 = (emptyS, b)
                | lengthS s == 1 = let 
                                        x = nthS s 0 
                                   in (singletonS b, f b x)
                | otherwise      = let 
                                        contract = contraccion f s
                                        (xs', last) = scanS f b contract
                                   in (expandir f s xs' 0, last)
                   
    fromList = (id)





