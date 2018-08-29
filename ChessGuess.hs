module Project2 (initialGuess, nextGuess, GameState) where
import Data.List

allPieces = ["BK","BQ","BR","BR","BB","BB","BN","BN","BP","BP","BP","BP","BP","BP","BP","BP",
             "WK","WQ","WR","WR","WB","WB","WN","WN","WP","WP","WP","WP","WP","WP","WP","WP"]
			 
-- get all possible combinations of n elements from a list haskell 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ xs !! i : x | i <- [0..(length xs)-1] 
                                  , x <- combinations (n-1) (drop (i+1) xs) ]
								  
								  
-- 	get all possible combinations from 1 to n elements from a list haskell							  
combinationTogether :: Int -> [a] -> [[a]]
combinationTogether 0 _ = [[]]
combinationTogether _ [] = [[]]
combinationTogether n (x:xs) = map (x:) (combinationTogether (n-1) xs) ++combinationTogether n xs

combinationFinal n list = nub (combinationTogether n list )

---------------------------
type GameState = [[String]]

-- initialGuess function
-- input a size and get the first guess and all the combinations from chess
initialGuess :: Int -> ([String],GameState)
initialGuess n = (["BK","BQ","BR","BB","BN"],combinationFinal n allPieces)


------------------------- Input the guess and GameState from the last guess 
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (guess,other) (n1,n2,n3) = (guessFinal (simple (guess,other) n1),(simple (guess,other) n1)) ---((head(other)),(simple (guess,other) n1))



------------------Delete the element in GameState if it contians any element from guess
simpleNoo :: [String] -> GameState -> GameState
simpleNoo [] (y:ys) = (y:ys)
simpleNoo [] lst = lst
simpleNoo guess [] = []
simpleNoo (x:xs) (y:ys)
        |elem x y  = simpleNoo (x:xs) ys
        |otherwise = if (x:xs) == [] then y:(simpleNoo xs (y:ys))
                     else simpleNoo xs (y:ys)
-----------------------

simple ::  ([String],GameState)-> Int -> GameState

-- When n1 == 0


simple (guess,[]) n1 = []
simple (guess,(y:ys)) 0 = simpleNoo guess (y:ys)
simple (guess,(y:ys)) n1 
    | (calculRight guess y) /= n1  = simple (guess,ys) n1
	| otherwise                    =  y:simple (guess,ys) n1   


calculRight :: [String] -> [String] -> Int
calculRight target guess = right
  where 
        common      = mintersect guess target
        right       = length common
		
mintersect :: Eq t => [t] -> [t] -> [t]
mintersect [] _ = []
mintersect (x:xs) r = if elem x r then x : mintersect xs (delete x r)
                      else mintersect xs r 
---------------------------------------------	    
-----------------------------------best guess


response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightKind, rightColor)
  where 
        common      = mintersect guess target
        right       = length common
        rguess      = foldr (delete) guess common
        rtarget     = foldr (delete) target common
        rightColor  = length $ mintersect (map (!!0) rguess) (map (!!0) rtarget)
        rightKind   = length $ mintersect (map (!!1) rguess) (map (!!1) rtarget)

					  
					  
					  
multiPlee ::  GameState -> [String] ->Int     --[String]
multiPlee allCombination n = length$nub(map(response n) allCombination)	

lengthAll allCombination = map(multiPlee allCombination) allCombination	


elementPosition :: Eq t => t -> [t] -> Int
elementPosition elt lst =  convertion lst 0
  where
    convertion [] _ = 0
    convertion (x:xs) n 
      | elt == x     = n
      | otherwise  = convertion xs (n+1)
	  
--[["BB","BQ"],["WQ"],["WQ","BK"],["BK","BQ","WK"]]
getPosition :: GameState -> Int	  
getPosition allCombination = elementPosition (maximum(lengthAll allCombination )) (lengthAll allCombination)


guessFinal allCombination = allCombination !!  getPosition allCombination


----------------------------------
----------------------------------
