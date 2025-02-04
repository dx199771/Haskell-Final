module Solitaire2 where

{- COM2108 Solitaire assignment: 2018 assignment 3
   NAMING CONVENTIONS
   p a pip value
   s a suit
   w1,w2 random weights
   bd a board
   (fnds,cols,res) foundations, columns, reserve
   colHeads - cards at heads of columns
   
-}

  import Solitaire1
  import Data.List
  import Data.Maybe
  import Debug.Trace
  import System.Random
  debug = flip trace
  
  topCard :: [Card] -> [Card]
  topCard [] = []
  topCard c
   |null t = [h]
   |not(isKing h) &&sCard h == head t = h: topCard ((head t):(tail t))
   |otherwise = [h]
   where (h:t)=c
    
  resToCol :: EOBoard -> EOBoard
  resToCol (fnds,cols,[]) = (fnds,cols,[])
  resToCol (fnds,cols,res)
    -- |null res = (newFnds,newCols,[])
    |elem hr (map pCard colHeads) = (fnds,updatedCols,tr)
    |otherwise = (newFnds,newCols,hr :newRes)
    where
     hr:tr = res
     colHeads = map head cols
     updatedCols = map(\n -> if pCard(head n)== hr then pCard(head n):n else n)cols
     (newFnds,newCols,newRes) = resToCol (fnds,cols,tr)
     
  colKingToCol :: EOBoard -> EOBoard
  colKingToCol (fnds,[],res) = (fnds,[],res)
  colKingToCol (fnds,cols,res)
    |length cols <8 && isKing (last moveAbleCards) && length moveAbleCards<length hc
    =(fnds,(drop(length moveAbleCards)hc):moveAbleCards:tc,res)
    |otherwise = (newFnds,hc:newCols,newRes)
    where
     hc:tc = cols
     moveAbleCards = topCard hc
     (newFnds,newCols,newRes) = colKingToCol (fnds,tc,res)
  
  resKingToCol :: EOBoard -> EOBoard
  resKingToCol (fnds,cols,[]) = (fnds,cols,[])
  resKingToCol (fnds,cols,res)
    |isKing hr && length cols <8 = (fnds,[hr]:cols,tr)
    |otherwise = (newFnds,newCols,hr:newRes)
    where
     hr:tr = res
     (newFnds,newCols,newRes) = resKingToCol (fnds,cols,tr)

  colToCol :: EOBoard -> EOBoard
  colToCol (fnds,[],res) = (fnds,[],res)
  colToCol (fnds,cols,res)
    |elem (last moveAbleCards) (map pCard colHeads) && ((8+1)-length res)>=length moveAbleCards
    =(fnds,updatedCols,res)
    |otherwise = (newFnds,hc:newCols,newRes) 
    where
     hc:tc = cols
     colHeads = map head tc
     moveAbleCards = topCard hc  --may move a list of cards, not only one
     updatedCols = drop(length moveAbleCards)hc:map(\n-> if pCard(head n)==last moveAbleCards then moveAbleCards++n else n)tc
     (newFnds,newCols,newRes) = colToCol (fnds,tc,res)

  colToResForward :: EOBoard -> EOBoard
  colToResForward (fnds,[],res) = (fnds,[],res)
  colToResForward (fnds,cols,res)
    |length hc >1 && elem (hc!!1) (map pCard colHeads) && length res <8= (fnds,(tail hc):tc,(head hc): res)
    |otherwise = (newFnds,hc:newCols,newRes)
    where
     hc:tc = cols
     colHeads = map head tc
     (newFnds,newCols,newRes) = colToResForward (fnds,tc,res)

  colToRes :: EOBoard -> EOBoard
  colToRes (fnds,[],res) = (fnds,[],res)
  colToRes (fnds,cols,res)
    |(8-length res) >= length moveAbleCards && last hc/=last moveAbleCards = (fnds, updatedCols ,moveAbleCards++res)
    |otherwise = (newFnds,hc:newCols,newRes)
    where
     hc:tc = cols
     moveAbleCards = topCard hc
     updatedCols = drop(length moveAbleCards)hc:tc
     (newFnds,newCols,newRes) = colToRes (fnds,tc,res)

       
  findMoves :: EOBoard -> [EOBoard]
  findMoves b = filter (\n -> n/=b) allMoves --eliminate duplicates
    where
    allMoves = (colKingToCol b:resKingToCol b:resToCol b:colToCol b:colToResForward b:colToRes b:[])
  
  chooseMove :: EOBoard -> Maybe EOBoard
  chooseMove b
    |null (findMoves b) = Nothing
    |otherwise = Just (head(findMoves b))
    
  eOGame :: EOBoard -> Int
  eOGame b1@(fnds,cols,res)
    |(null cols1)&&(null res1) = 52
    |chooseMove (toFoundations b)== Nothing = (52- (length res) - (foldr (+) 0 (map length cols)))
    |otherwise = eOGame (resMaybe1(chooseMove (toFoundations b))) `debug` show(resMaybe1(chooseMove (toFoundations b)))
    where
     notEmpty = (filter (not.null) cols)
     b = (fnds,notEmpty,res)
     (fnds1,cols1,res1) = toFoundations b    
    
    
  resMaybe1 :: (Maybe a) -> a
  resMaybe1 (Just x) = x 

  generateRandomSet :: Int->[Int]
  generateRandomSet int = take 100(randoms(mkStdGen int)::[Int])
  
  eOExpt:: [Int] -> [Int] 
  eOExpt set
    |null set = []
    |otherwise = eOGame (eODeal (head set)):eOExpt (tail set)
  
  meanStats :: [Int]->(Float,Float,Float)
  meanStats lis = (mean,var,sqrt var) 
   where
    rlis = map fromIntegral lis
    rlen = fromIntegral (length lis)
    mean = (foldr (+) 0 rlis)/rlen
    var = (foldr (+) 0 (map (\ n -> (n-mean)**2) rlis))/rlen
    