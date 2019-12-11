{-# LANGUAGE OverloadedStrings #-}

module HtmlBody
    ( getBodyTag
    , newSess
    , getResBody
    , getRes
    , getBody
    ,
    ) where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 as C
import           Network.Wreq
import qualified Network.Wreq.Session       as Sess
import           Text.HTML.TagSoup

type Res = Response C.ByteString

newSess :: IO Sess.Session
newSess = Sess.newSession

getRes :: Sess.Session -> String -> IO Res
getRes sess url = do
    Sess.get sess url

getBody :: Res -> IO C.ByteString
getBody r = do
    return $ r ^. responseBody

getBodyTag :: C.ByteString -> IO [Tag C.ByteString]
getBodyTag r = do
    return $ parseTags r

getResBody :: Sess.Session -> String -> IO C.ByteString
getResBody sess url = do
    r <- Sess.get sess url
    return $ r ^. responseBody
