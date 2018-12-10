module MeanStats where

 -- mean, variance, sd of a list of Ints
 
 
 meanStats :: [Int]->(Float,Float,Float)
 
 meanStats lis = (mean,var,sqrt var) 
  where
   rlis = map fromIntegral lis
   rlen = fromIntegral (length lis)
   mean = (foldr (+) 0 rlis)/rlen
   var = (foldr (+) 0 (map (\ n -> (n-mean)**2) rlis))/rlen

   
   
     addItemToCol ::[Card] -> EOBoard -> EOBoard
  addItemToCol c b
    |otherwise = (fnds,c:cols,res)
    where 
     (fnds,cols,res) = b