-- Author: Jiaqi Zhuang
--Student ID: 1122155
-- Purpose: implement functions and algorithms to the music guessing
-- game and aiming to guess the right chord as less time as possible.

-- Game of Musician is basially selecting a random chord and player
-- has to guess the right chord in the minimum amount of times.
-- A chord consists of three pitches and each pitch contain a note
-- (between A to G) and an octave (between 1 to 3). Every time when
-- player is guessing, the feedback function would give a result
-- which shows that how many pitch/octave/note player guessed
-- correctly. Base on this information, player can apply some
-- expectation & consistency strategies to discard some incorrect
-- chords. Details are provided in NextGuess and getChordExp functions.

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where
              
import Data.List

----------------------------to pitch-------------------------------
-- convert a input String into a maybe Pitch type. 
data Pitch = Pitch String
toPitch :: String -> Maybe Pitch
toPitch [] = Nothing
toPitch pitch
    | elem (head pitch) ['A'..'G'] && elem (tail pitch) ["1","2","3"]
    && length(pitch) == 2 = Just (Pitch pitch)
    | otherwise = Nothing


-- show the maybe pitch output as string type
instance Show Pitch where show = showPitch
showPitch :: Pitch -> String
showPitch (Pitch pitch) = pitch


-----------------------------feedback------------------------------
-- Feedback takes two lists (target and guess) as input. 
-- removeSamePitch removes the pitches which appeared in both 
-- target chord and guess chord.
-- First time using match_elem to get the number of pitches.
-- Second time calling match_elem to get the number of same notes
-- of guess and target chords by comparing the letters.
-- Third time calling match_elem to get the number of same octaves
-- by comparing integers. 

feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback pitch_target pitch_guess = 
    let removeSamePitch xs ys = filter (\x -> not (x `elem` ys)) xs 
    in (match_elem target guess, 
    match_elem (map head (removeSamePitch target guess)) 
             (map head (removeSamePitch guess target)),
    match_elem (map tail (removeSamePitch target guess))
             (map tail (removeSamePitch guess target)))
    where target = toStringList pitch_target
          guess = toStringList pitch_guess
          
          
-- match_elem takes two lists as input and return the total number 
-- of elements appeared in both lists.
match_elem :: Eq a => [a] -> [a] -> Int
match_elem _ [] = 0
match_elem xs (y:ys)
    | elem y xs = 1 + match_elem (delete y xs) ys
    | otherwise = match_elem xs ys


-- convert a list of pitch to a list of string
toStringList :: [Pitch] -> [String]
toStringList [] = []
toStringList (x:xs) = (toStringElem x : toStringList xs)


-- convert a pitch to a string
toStringElem :: Pitch -> String
toStringElem (Pitch x) = x


--------------------------initial guess----------------------------
-- make an initial guess. Store all the 1330 chords in the GameState.
type GameState = [[Pitch]]
initialGuess :: ([Pitch],GameState)
initialGuess = ([Pitch "A1", Pitch "C1", Pitch "E2"], chords)
    where pitch = [[note, octave]|note<-['A'..'G'], octave<-['1'..'3']]
          chords = permutation pitch


--find all possible combinations of three notes.
permutation :: [String] -> [[Pitch]]
permutation xs = [[Pitch x,Pitch y,Pitch z] | 
                    x<-xs, y<-xs, z<-xs, x<y, y<z] 


----------------------------next guess-----------------------------
-- Everytime after made a guess, use the guess as target to match
-- with the other chords (i.e. the GameState). Only keeps the chords
-- that has the same result as previous guessing feedback and delete
-- rest of the chords (called nextState). This can be used to reduce 
-- the number of guessings for the next guess.
-- In addition, expectation method is applied here.  
-- The process is:
-- Assume there are N chords left in the GameState. 
-- Select each chord in GameState to be the target and let all 
-- remaining chords(include the selected chord) in GameState to be 
-- the guess, so that use feedback function to get N new feedbacks
-- for each chord. In other word, N chords would have N targets,
-- N*N guesses and N*N feedbacks. Then, we use the expectation
-- formula (freq/total*freq) to get N minimum expected values in total,
-- and we finally guess the chord with the lowest expected value.
nextGuess :: ([Pitch],GameState)->(Int,Int,Int)->([Pitch],GameState)
nextGuess (lastGuess, state) guessAns = (guess, nextState)
    where nextState = [x|x<-state, (feedback x lastGuess)==guessAns]
          guess = head [chord | chord<-nextState, 
                  axisEqual (getChordExp chord nextState)
                  (minimum (allChordExp nextState nextState))]
                  

-- allChordExp takes a list of N chords as input and returns a list
-- of N expected values, which can be used to find the minimum expected
-- value in the next Guess function.
allChordExp :: GameState -> GameState -> [Double]
allChordExp [] _ = []
allChordExp (x:xs) ys = getChordExp x ys : allChordExp xs ys


-- getChordExp takes one chord (from GameState) as input as well as
-- the whole GameState. It will return its corresponding expected
-- guess value. The logic here is: we call feedback function N times 
-- to get N feedbacks, then group the same feedbacks together to find
-- the frequency of each feedbacks for the input chord.
getChordExp :: [Pitch] -> GameState -> Double
getChordExp [] _ = 0.0
getChordExp chord ys 
    = (getExp (getFeedbackFreq (group chordfeedbacks)) 
              (fromIntegral (length chordfeedbacks)))
    where chordfeedbacks = (sort [feedback chord y| y<-ys])


-- count frequency of each feedback.
getFeedbackFreq :: [[(Int, Int, Int)]] -> [Int]
getFeedbackFreq [] = []
getFeedbackFreq (x:xs) = length x : getFeedbackFreq xs
    
    
-- use the sum of frequency / totalLength * frequency to get
-- the expected guess value. (hint 6)
getExp :: [Int] -> Double -> Double
getExp [] _ = 0.0
getExp (freq:xs) total 
    = (fromIntegral freq / total) * (fromIntegral freq)
    + getExp xs total


-- float comparison for equality
axisEqual :: (Eq a, Num a, Ord a, Fractional a) => a -> a -> Bool
axisEqual  a b
  | (abs (a - b)) <= 0.011 = True
  | otherwise      = False   