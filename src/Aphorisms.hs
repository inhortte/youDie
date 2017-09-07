module Aphorisms
  ( doleOut
  , randomFromList
  , sentenceFromFile
  , eligeSentenceFromBlog
  ) where

import Control.Monad (mapM, forM)
import System.Random (randomR, randomRs, getStdGen, newStdGen)
import Data.Int
import System.FilePath.Posix
import System.Directory
import System.IO
import Text.Regex.Posix

blogDir :: FilePath
blogDir = "/home/polaris/Dropbox/archiv/martenblog"

blogRegex :: String
blogRegex = "[A-Z][[:alpha:][:space:],;:\\\"']+[\\.]"

aphorisms :: [String]
aphorisms = [
  "You, die."
  , "Slap the trollop."
  , "Freak."
  , "The scabs are peeling away from your wound."
  , "Forget her, vole."
  , "There are three types of women in praha."
  , "Nasrat, vole."
  , "You are under the influence of your weighty past."
  , "What is your favourite flavour of ice cream, vole?"
  , "I sucked down too much goat juice."
  , "You DIE!"
  , "You have died."
  , "Gargle."
  , "Repetition is a form of change."
  , "Women are the failure of evolution to cope with parasitic scourge."
  , "Assuming is half the battle."
  , "Think of yourself as shining her coal-face into a diamond."
  , "Three hamsters for every pair of scissors."
  , "She was walking through high grass when the invasion came."
  , "Wake up, jaw-whore"
  , "It's just a hole."
  , "We stay long after the motif is stated to hear its muddied eddies."
  , "Common knowledge is only locally common." ]

opening :: [String]
opening = [
  "You lie on the scabbed pavement."
  , "The bar closed over twenty minutes ago."
  , "You fall off the edge of your rumpled matress."
  , "The envelope the old lady slipped under your door is almost empty." ]

rustling :: [String]
rustling = [
  "Every third tick of the clock pulses with your hangover."
  , "Rancid chunks of tuna are scattered in a semicircle."
  , "A cup of urine sloshes gently a few decimetres out of your reach."
  , "A pool of saliva forms a glistening moon eclipsed by your stubble."
  , "Her last facial expression flickers in your memory."
  , "Your left hand is balled into a fist you cannot unclench." ]

thunder :: [String]
thunder = [
  "You rub the raw patch on your belly, moaning."
  , "A rat sinks its mutant fangs into your armpit."
  , "Your mobile begins to yowl."
  , "The thought of the cleaning woman that you practically raped the other day arriving puts you in motion."
  , "Fortunately, you have three beers and a bottle of cheap brandy in your backpack." ]

stillness :: [String]
stillness = [
  "At first, you don't notice the needle enter your skull"
  , "A seizure wracks your lower body, leaving your paralized."
  , "Images of whirling skirts edged with razors dance."
  , "A pot of boiling liquid plummets." ]

death :: [String]
death = [
  "You die."
  , "You leave yourself to rot."
  , "It may be tomorrow's dream." ]

randomPhrase :: [String] -> IO String
randomPhrase pColl = do
  thurk <- (getStdGen >>= (\gen -> return . head . map (pColl !!) . take 1 . randomRs (0, length pColl - 1) $ gen))
  newStdGen
  return thurk

randomFromList :: [a] -> IO a
randomFromList xs = do
  item <- (getStdGen >>= (\gen -> return . (xs !!) . fst . randomR (0, length xs) $ gen))
  newStdGen
  return item

eliminateNewlines :: String -> String
eliminateNewlines = map (\c -> if c == '\n' then ' ' else c) . unlines . drop 5 . lines

isolateSentences :: String -> [String]
isolateSentences contents = getAllTextMatches $ contents =~ blogRegex :: [String]

sentenceFromFile :: FilePath -> IO String
sentenceFromFile fp = readFile fp >>= (\contents -> return $ isolateSentences . eliminateNewlines $ contents) >>= randomFromList

eligeSentenceFromBlog :: IO String
eligeSentenceFromBlog = listDirectory blogDir >>= randomFromList >>= (\filename -> sentenceFromFile (blogDir </> filename))
  
{-
assembleStory :: [[String]] -> IO [String]
assembleStory phraseCollections = do
  forM phraseCollections (\pColl r
  spung <- (getStdGen >>= (\gen ->
                             mapM (\pColl ->
                                     fst .
                                     randomR (0, length pColl - 1) $ gen) [1,2]
  return $ foldlM (\b s -> b ++ " " ++ s) "" spung
-}

doleOut :: [Int32] -> IO [String]
doleOut ims = do
  return ["thurk?", "bastard?"]
