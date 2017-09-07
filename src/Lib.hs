{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( youDie, whackMe, yurt
    ) where

import DBus
import DBus.Client
import Data.Int
import Control.Monad (mapM, mapM_)
import Aphorisms (eligeSentenceFromBlog)

whackMe :: Client -> String -> [String] -> IO [Int32]
whackMe client method args = do
  reply <- call_ client (methodCall (objectPath_ "/im/pidgin/purple/PurpleObject") (interfaceName_ "im.pidgin.purple.PurpleInterface") (memberName_ method))
    { methodCallDestination = Just $ busName_ "im.pidgin.purple.PurpleService"
    , methodCallBody = map toVariant args
    }
  let Just res = fromVariant (methodReturnBody reply !! 0)
  return $ map (\x -> x :: Int32) (res :: [Int32])

yurt :: Client -> String -> [Int32] -> IO Int32
yurt client method args = do
  reply <- call_ client (methodCall (objectPath_ "/im/pidgin/purple/PurpleObject") (interfaceName_ "im.pidgin.purple.PurpleInterface") (memberName_ method))
    { methodCallDestination = Just $ busName_ "im.pidgin.purple.PurpleService"
    , methodCallBody = map toVariant args
    }
  let Just res = fromVariant (methodReturnBody reply !! 0)
  return $ (res :: Int32)

toga :: Client -> String -> Int32 -> String -> IO Int32
toga client method convChat sentence = do
  reply <- call_ client (methodCall (objectPath_ "/im/pidgin/purple/PurpleObject") (interfaceName_ "im.pidgin.purple.PurpleInterface") (memberName_ method))
    { methodCallDestination = Just $ busName_ "im.pidgin.purple.PurpleService"
    , methodCallBody = [ toVariant convChat, toVariant sentence ]
    }
  let Just res = fromVariant (methodReturnBody reply !! 0)
  return $ (res :: Int32)

youDie :: IO ()
youDie = do
  client <- connectSession
  ims <- whackMe client "PurpleGetIms" []
  putStrLn "Ims:"
  mapM_ (\im -> putStrLn $ show im) ims
  convChats <- mapM (\im -> yurt client "PurpleConversationGetImData" [im]) ims
  putStrLn "convChats:"
  mapM_ (\convChat -> putStrLn $ show convChat) convChats
  sentence <- eligeSentenceFromBlog
  putStrLn sentence
  mapM_ (\convChat -> toga client "PurpleConvImSend" convChat sentence) convChats
  putStrLn "Death!"
