{-# LANGUAGE OverloadedStrings #-}

module Profile.HentaiNexus
    ( getArtist
    , getStoryUrls
    , getStoryName
    , getPageNumber
    , getImageUrl
    , getImageName
    , saveStory
    ) where

import           Data.ByteString.Lazy.Char8 as C
import           Data.List.Split
import           HtmlBody
import           Network.Wreq
import           Network.Wreq.Session       as Sess
import           System.Directory
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Regex.TDFA

host = "https://hentainexus.com"

getArtist :: [Tag C.ByteString] -> String
getArtist tags = do
    let a = Prelude.head $ Prelude.filter (tagText isArtistName) tags
    let a2 = extractText a =~ (":[a-zA-Z0-9]+"::String) :: String
    Prelude.tail a2

extractText :: Tag C.ByteString -> String
extractText (TagText a) = C.unpack a

isArtistName :: C.ByteString -> Bool
isArtistName t = t =~ ("artist:[a-zA-Z0-9]+"::String) :: Bool

getStoryUrls :: [Tag C.ByteString] -> [String]
getStoryUrls tags = do
    let rs = Prelude.filter ( tagOpen (=="a") hasStoryUrl ) tags
        has = Prelude.map (fromAttrib "href") rs
        vsl = Prelude.filter validateStoryUrl has
        bts = Prelude.map C.unpack vsl
    Prelude.map (host++) bts

validateStoryUrl :: C.ByteString -> Bool
validateStoryUrl s = s =~ ("/view/[0-9]+"::String)::Bool

hasStoryUrl :: [Attribute C.ByteString] -> Bool
hasStoryUrl as = do
   Prelude.any isStoryUrl as

isStoryUrl :: Attribute C.ByteString -> Bool
isStoryUrl a = (=="href") $ C.unpack $ fst a

getStoryName :: [Tag C.ByteString] -> String
getStoryName tags = do
    let msn = fromTagText
            $ Prelude.head
            $ Prelude.filter (tagText isStoryName) tags
        sn  = msn =~ ("[A-Z][a-zA-Z0-9[:space:]~!-]+" :: String)
    C.unpack sn

isStoryName :: C.ByteString -> Bool
isStoryName  n = n =~ (".::+"::String)::Bool

getPageNumber :: [Tag C.ByteString] -> Int
getPageNumber tags =
    let pageUrl = Prelude.filter ( tagOpen (=="a") (hasPageUrl) ) tags
    in Prelude.length pageUrl

hasPageUrl :: [Attribute C.ByteString] -> Bool
hasPageUrl as =
    Prelude.any isPageUrl as

isPageUrl :: Attribute C.ByteString -> Bool
isPageUrl a =
    (snd a) =~ ("/read/[0-9]+/[0-9]+"::String) :: Bool

getPageUrl :: [Tag C.ByteString] -> [String]
getPageUrl tags = do
    let purl = Prelude.map (host++)
             $ Prelude.map (C.unpack)
             $ Prelude.map (fromAttrib "href")
             $ Prelude.filter (tagOpen (=="a") hasPageUrl) tags
    purl

getImageUrl :: [Tag C.ByteString] -> String
getImageUrl tags = do
    let it = Prelude.head $ Prelude.filter (tagOpen (=="img") (\a ->Prelude.any (==("id","currImage")) a)) tags
        il = fromAttrib "src" it
    C.unpack il

getImageName :: String -> String
getImageName i = i =~ ("[0-9]+\\.[a-z]+"::String)::String

createArtistDir :: IO ()
createArtistDir = Prelude.putStr ""

saveImage :: IO ()
saveImage = Prelude.putStr ""

saveStory :: String -> IO ()
saveStory url = do
    sess <- newSess
    r <- getRes sess url
    b <- getBody r
    bt <- getBodyTag b
    let sn = getStoryName bt
        spurl = getPageUrl bt
    savePageImages sess spurl

savePageImages :: Sess.Session -> [String] -> IO ()
savePageImages _ [] = print "Finish"
savePageImages sess (x:s) = do
    r <- getRes sess x
    b <- getBody r
    bt <- getBodyTag b
    let imageUrl = getImageUrl bt
        imageName = getImageName imageUrl
    ri <- getRes sess imageUrl
    bi <- getBody ri
    C.writeFile imageName bi
    savePageImages sess s
