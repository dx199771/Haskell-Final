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
  
  --data Maybe a = Nothing | Just a
  --                   deriving(Eq,Ord,Read,Show)
  --import EOIO
  topCard :: [Card] -> [Card]
  topCard c
   |null t = [h]
   |not(isKing h) &&sCard h == head t = h: topCard ((head t):(tail t))
   |otherwise = [h]
   where (h:t)=c
   --findMoves :: EOBOard -> [EOBoard]

--move moveable reserve to colomn
  resToCol :: EOBoard -> EOBoard
  resToCol (fnds,cols,res)
   |null res =([],[],[])
   |elem hr pColHeads = updatedBoard
   |otherwise = ([],[],[])
   where
     updatedBoard = (fnds,(map(\n -> if (head n == sCard hr) then (hr:n) else n)cols),tr)
     pColHeads = map pCard (map head cols)
     rearrnge = filter (\n -> elem n pColHeads)res ++ filter (\n -> notElem n pColHeads)res
     (hr:tr) = rearrnge

--move king from reserve to empty colomn
  resKingToCol :: EOBoard -> EOBoard
  resKingToCol (fnds,cols,res)
    |null (filter isKing res)= ([],[],[])
    |isKing hr && length cols <8 =(fnds,[hr]:cols,tr)
    |otherwise = ([],[],[])
    where
     rearrange = filter isKing res ++ filter (not.isKing) res
     hr:tr = rearrange

  colKingToCol :: EOBoard -> EOBoard
  colKingToCol (fnds,cols,res)
    |null (filter (isKing.last.topCard)cols) = ([],[],[])
    |(length cols) <8 && (length colHeads /= length (head rearrange)) = (fnds,colHeads :updatedCol:tc,res) 
    |otherwise = ([],[],[])
    where
     colHeads = topCard (head rearrange)
     rearrange = filter (isKing.last.topCard ) cols ++ filter (not.isKing.last.topCard) cols
     hc:tc = rearrange
     updatedCol = drop (length colHeads) hc 

  colToCol :: EOBoard -> EOBoard
  colToCol (fnds,cols,res)
    |notElem(last colHeads)(map pCard(map head tc))= ([],[],[])
    |length colHeads <= (8+1)-length res = 
    (fnds,updatedCol:map(\n-> if( pCard (head n) == last colHeads )then colHeads ++ n else n)tc,res) 
    |otherwise =([],[],[])
    where
     hc:tc = rearrange
     colHeads = topCard (head rearrange)
     colHeadsPre = map pCard (map head cols)
     rearrange = filter(\n-> elem (last(topCard n))colHeadsPre)cols ++ filter(\n-> notElem (last(topCard n))colHeadsPre)cols
     updatedCol = drop (length colHeads) hc 

  colToRes :: EOBoard -> EOBoard
  colToRes (fnds,cols,res)
    |null (filter (\n -> (elem (n!!1) colHeadPcard) )moveAbleCol) = ([],[],[])
    |length res <8 && length(topCard hc) ==1 && (elem (hc!!1)succFoundations || elem(hc!!1) colHeadPcard) 
    =(fnds,(tail hc):tc,(head hc):res)
    |otherwise = ([],[],[])
    where 
     hc:tc = rearrange
     succFoundations = map sCard (filter (not.isKing) fnds)
     colHeadPcard=(map pCard(map head cols))++(map sCard (filter (not.isKing) res))
     rearrange = filter(\n-> (elem (n!!1)colHeadPcard))moveAbleCol ++ filter(\n-> (notElem (n!!1)colHeadPcard))moveAbleCol++filter (\n -> (length n <=1))cols 
     moveAbleCol = filter (\n -> (length n >1))cols  --find colomn has more than one card

  colToRes2 :: EOBoard -> EOBoard
  colToRes2 (fnds,cols,res)
    |null (filter (\n -> (elem (n!!2) colHeadPcard) )moveAbleCol) = ([],[],[])
    |length res <7 && length(topCard hc) ==1 && (elem (hc!!2)succFoundations || elem(hc!!2) colHeadPcard) 
    =(fnds,(drop 2 hc):tc,(take 2 hc)++res)
    |otherwise = ([],[],[])
    where 
     hc:tc = rearrange
     succFoundations = map sCard (filter (not.isKing) fnds)
     colHeadPcard=(map pCard(map head cols))++(map sCard (filter (not.isKing) res))
     rearrange = filter(\n-> (elem (n!!2)colHeadPcard))moveAbleCol ++ filter(\n-> (notElem (n!!2)colHeadPcard))moveAbleCol++filter (\n -> (length n <=1))cols 
     moveAbleCol = filter (\n -> (length n >2))cols  --find colomn has more than one card

  colToRes1 :: EOBoard -> EOBoard
  colToRes1 (fnds,cols,res)
    |(isKing (last hc))==True = ([],[],[]) 
    |length colHeads <= (8+1)-length res 
    =(fnds,updatedCol:tc,colHeads ++res)
    |otherwise = ([],[],[])
    where 
     hc:tc = rearrange
     rearrange = moveAbleCol++(filter(\n-> notElem n moveAbleCol )cols)
     moveAbleCol = (filter (\n -> (not.isKing.last.topCard) n &&length(topCard n)<=8-(length res)) cols)
     colHeads = topCard (head rearrange)
     updatedCol = drop (length colHeads) hc 
-- ++ [colKingToRes b]
  findMoves :: EOBoard -> [EOBoard]
  findMoves b1
    |null (filter(\n -> n/=([],[],[])) ([colKingToCol b]++[resKingToCol b]++[resToCol b]  ++[colToCol b] ++[colToRes b]++[colToRes2 b]++[colToRes1 b])) = []
    |otherwise = filter(\n -> n/=([],[],[])) ([colKingToCol b]++[resKingToCol b]++[resToCol b]++[colToCol b]  ++[colToRes b]++[colToRes2 b]++[colToRes1 b])
    where
     b= toFoundations b1
  chooseMove ::  EOBoard -> Maybe EOBoard
  chooseMove b
    |null (findMoves b) || head (findMoves b) == b= Nothing
    |otherwise = Just (head (findMoves b))

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
