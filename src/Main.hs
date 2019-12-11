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
    t <- getBodyTag sess testStory
    print $ show $ getPageNumber t


