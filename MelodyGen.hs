module MelodyGen where

import Data.List
import Data.Random
import Data.RVar
import Data.Random.Distribution.Poisson
import Data.Random.Distribution.Categorical
import Control.Monad
import System.Random
import Data.Maybe

samplePoisson :: Double -> IO Int
samplePoisson lambda = sampleRVar $ poisson lambda

genEighthRhythm :: IO [(Rhythm,Length)]
genEighthRhythm = do
  rest <- sampleRVar $ categorical [(0.5 :: Double, []),(0.5, [(Rest,FirstEigth)])]
  number <- samplePoisson 2
  let res = flip map [1..1+number] $ \x -> if (x+(length rest))`mod`2 == 0
                                           then (Beat,SkipEigth) else (Beat,FirstEigth)
  let postrest = if (number+(length rest))`mod`2 == 0 then [(Rest,SkipEigth)] else []
  return $ rest ++ res ++ postrest
genQuarterRhythm = do
  rest <- sampleRVar $ categorical [(0.5 :: Double, []),(0.5, [(Rest,FirstEigth)])]
  number <- samplePoisson 2
  let res = flip concatMap [1..1+number] $ const $ if null rest then [(Beat,Quarter)] else [(Beat,SkipEigth),(Rest,FirstEigth)]
  return $ rest ++ (if null rest then res else init res)
genEighthPartialRhythm = do
  rest <- sampleRVar $ categorical [(0.5 :: Double, []),(0.5, [(Rest,FirstEigth)])]
  number <- samplePoisson 2
  let res = flip concatMap [1..1+number] $ \x -> if (x+(length rest))`mod`2==0
                                                 then [(Beat,SkipEigth),(Beat,FirstEigth),(Rest,SkipEigth)]
                                                 else [(Beat,FirstEigth),(Beat,SkipEigth),(Rest,FirstEigth)]
  b <- randomIO
  let res'' = if b then rest ++ res else map (\(b,c) -> (invertRhythm b,c)) $ rest ++ res
      res' = if (not b) && (null rest) then drop 2 res'' else res''
  return $ case (last res') of
    (Rest,FirstEigth) -> init res'
    (Beat,FirstEigth) -> init res' ++ [(Beat,Quarter)]
    otherwise -> res'
genTripletRhythm = do
  rest <- sampleRVar $ categorical [(1.0/3 :: Double, []),(2.0/3, [(Rest,Triplet)])]
  number <- fmap (max 1) $ samplePoisson 2
  let res = flip map [1..number] $ const (Beat,Triplet)
      nrests = if (number+(length rest))`mod`3 >0 then 3- (number+(length rest))`mod`3 else 0
  let postrest = flip map [1..nrests] $ const (Rest,Triplet)
  return $ rest ++ res ++ postrest
genTripletPartialRhythm = do
  rest <- sampleRVar $ categorical [(1.0/3 :: Double, []),(2.0/3, [(Rest,Triplet)])]
  number <- samplePoisson 2
  let res = flip concatMap [1..number+1] $ const [(Beat,Triplet),(Beat,Triplet),(Rest,Triplet)]
  return $ rest ++ (if null rest then res else init res)
genBrokenTripletRhythm = do
  rest <- sampleRVar $ categorical [(1.0/3 :: Double, []),(1.0/3, [(Rest,Triplet)]),(1.0/3, [(Rest,Triplet),(Rest,Triplet)])]
  number <- fmap (+2) $ samplePoisson 1
  let res = flip concatMap [1..number] $ const [(Beat,Triplet),(Rest,Triplet)]
  case (2*number + (length rest))`mod`3 of
    0 -> return $ rest ++ res
    1 -> return $ rest ++ (init res)
    2 -> return $ rest ++ res ++ [(Rest,Triplet)]
genSixteenthRhythm = do 
  rest <- sampleRVar $ categorical [(1.0/2 :: Double, []),(1.0/2, [(Rest,Sixteenth),(Rest,Sixteenth)])]
  number <- fmap (+1) $ samplePoisson 1
  let res = flip concatMap [1..number] $ const [(Beat,Sixteenth),(Beat,Sixteenth)]
  b <- randomIO :: IO Bool
  let endR = case (b,even $ number + ((length rest)`div`2)) of
        (True,True) -> [(Beat,Quarter)] 
        (True,False) -> [(Beat,Sixteenth),(Rest,Sixteenth)]
        (False,False) -> [(Rest,Sixteenth),(Rest,Sixteenth)]
        (False,True) -> []
  return $ rest ++ res ++ endR
  

testGen n f = do
  g <- fmap (map $ foldl' (+) 0 . map (\(_,r) -> rvalue r)) $ forM [1..n] $ const f
  return $ map ((==0) . (`mod`12)) g

data Rhythm = Rest | Beat deriving (Eq,Show)
data Length = Quarter | FirstEigth | SkipEigth | Sixteenth | Triplet deriving (Eq,Show)


rvalue Quarter = 12
rvalue FirstEigth = 8
rvalue SkipEigth = 4
rvalue Sixteenth = 3
rvalue Triplet = 4

invertRhythm Rest = Beat
invertRhythm Beat = Rest

genSimpleRhythm n = fmap concat $ forM [1..n] $ \_ -> do
  beatsOfRest <- samplePoisson 2
  notes <- join $ sampleRVar $ categorical [(0.2 :: Double, genEighthRhythm),
                                            (0.2, genEighthPartialRhythm),
                                            (0.2, genQuarterRhythm),
                                            (0.1, genTripletRhythm),
                                            (0.1, genTripletPartialRhythm),
                                            (0.05, genBrokenTripletRhythm),
                                            (0.05, genSixteenthRhythm)]
  return $ (replicate beatsOfRest (Rest,Quarter)) ++ notes
  

iidArrange :: [(Double,Int)] -> [(Rhythm,Length)] -> IO [(Maybe Int,Length)]
iidArrange probs rtm = mapM (\(x,l) -> case x of
                                           Rest -> return (Nothing,l)
                                           Beat -> fmap (\y -> (Just y,l)) $ sampleRVar $ categorical probs)
                       rtm

genSimpleMelodies2Voice = do
  fmap (takeAccumulator 48) $ (iidArrange [(0.75,1),(0.25,2)]) =<< (genSimpleRhythm 1000)

sortByEnergy2Voice mels = let energies = map (energy [(1,1),(2,1.5)]) mels
                          in map snd $ sortBy (compare `on` fst) $ zip energies mels
     

takeAccumulator n l = let it = ta 0 l
                      in if (sum $ map (rvalue.snd) $ last it) == n then it else init it
  where ta a [] = [[]]
        ta a (l:ls)
          | a == n    = []:(ta 0 $ l:ls)
          | otherwise = let (b:bs) = ta (a+(rvalue $ snd l)) ls in (l:b):bs
                                                                   
a//b = (fromIntegral a)/(fromIntegral b)

fst3 (a,b,c) = a
(f `on` g) x y = f (g x) (g y)


actualLengthDecomp l = gpd
  where gpd = map (\l@((a,_):_) -> (a,sum $ map (rvalue.snd) l)) $ groupBy (\_ (b,_) -> b == Nothing) l

positionDecomp l = gpd
  where gpd = snd $ mapAccumL (\a (x,l) -> (a+l,(x,a,l))) 0 $ actualLengthDecomp l

positionPoints pos len = case (pos `mod` 12,len) of 
  (0,_) -> 0.0
  (8,4) -> 0.0 
  (8,l) 
    | l >= 8 -> 3
  (4,4) -> 0.0
  (4,l)
    | l >= 8 -> 4.0
  (x,_)
    | x`mod`3==0 -> 2.0

actualLengthEnergy points l = sum $ 
                              map (\(Just i,x) -> (fromJust $ lookup i points) * 12//(max 12 x)) $ 
                              filter ((/=Nothing).fst) $ actualLengthDecomp l
positionEnergy points l = sum $ 
                          map (\(Just i,x,y) -> (fromJust $ lookup i points) * (positionPoints x y)) $ 
                          filter ((/=Nothing).fst3) $ positionDecomp l
                          
energy points l = (actualLengthEnergy points l) + (positionEnergy points l)

singleVoice voice rhythm = if (fst $ head dc) == Nothing 
                           then map snd dc 
                           else 0:(map snd dc)
  where dc = actualLengthDecomp $ map (\(a,b) -> (if a == (Just voice) then a else Nothing,b)) rhythm

toJSON :: Int -> [[(Maybe Int, Length)]] -> String
toJSON voices rhythms = theprestuff ++ thelist ++ "\n}"
  where flips = map (\r -> "      {\n        \"duration\" : 1 ,\n" ++ 
                           (concat $ intersperse " ,\n" 
                            $ map (\v -> "        \"voice" ++ (show v) ++ "\" : " ++ (show $ singleVoice v r)) [1..voices])
                           ++ "\n      }" ) rhythms
        thelist = "  \"variations\" :\n    [\n" ++ (concat $ intersperse " ,\n" flips) ++ "\n    ]"
        theprestuff = "{\n  \"voices\" : " ++ (show $ map (("voice"++).show) [1..voices]) ++ " ,\n"
                      ++ "  \"duration\" : 4 ,\n  \"signature\" : 12 ,\n" 
                      
saveMelodyJSON2Voice = writeFile "swing/time-melody-2voice.json" =<< (fmap ((toJSON 2) . sortByEnergy2Voice) $ genSimpleMelodies2Voice)