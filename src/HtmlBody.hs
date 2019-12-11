{-# LANGUAGE OverloadedStrings #-}

module HtmlBody
    ( getBodyTag
    , newSess
    , getResBody
    ) where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 as C
import           Network.Wreq
import qualified Network.Wreq.Session       as Sess
import           Text.HTML.TagSoup

newSess :: IO Sess.Session
newSess = Sess.newSession

getBodyTag :: Sess.Session -> String -> IO [Tag C.ByteString]
getBodyTag sess url = do
    r <- Sess.get sess url
    let rb = r ^. responseBody
    return $ parseTags rb

getResBody :: Sess.Session -> String -> IO C.ByteString
getResBody sess url = do
    r <- Sess.get sess url
    return $ r ^. responseBody
