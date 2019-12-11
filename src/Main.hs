{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Lens
import           Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import qualified Data.Text                  as T
import           Debug.Trace
import           HtmlBody
import           Network.Wreq
import qualified Network.Wreq.Session       as Sess
import           Profile.HentaiNexus
import           System.Directory
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Regex.TDFA

host = "https://hentainexus.com"
testLink = "https://hentainexus.com/?q=artist:Akinosora"
testStory = "https://hentainexus.com/view/6182"
testPage = "https://hentainexus.com/read/6182/001"

main :: IO ()
main = do
    sess <- newSess
    r <- getRes sess testStory
    b <- getBody r
    bt <- getBodyTag b
    -- let imgN = getImageUrl t
    -- irb <- getResBody sess imgN
    -- C.writeFile (getImageName imgN) irb
    createDirectoryIfMissing True $ getStoryName bt

