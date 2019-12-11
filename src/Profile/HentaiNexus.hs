{-# LANGUAGE OverloadedStrings #-}

module Profile.HentaiNexus
    ( getArtist
    , getStoryUrls
    , getStoryName
    , getPageNumber
    , getImageUrl
    , getImageName
    ) where

import           Data.ByteString.Lazy.Char8 as C
import           Data.List.Split
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
getStoryName tags = ""

getPageNumber :: [Tag C.ByteString] -> Int
getPageNumber tags =
    let pageUrl = Prelude.filter ( tagOpen (=="a") (hasPageUrl) ) tags
    in Prelude.length pageUrl

hasPageUrl :: [Attribute C.ByteString] -> Bool
hasPageUrl as = do
    Prelude.any isPageUrl as

isPageUrl :: Attribute C.ByteString -> Bool
isPageUrl a = do
    (snd a) =~ ("/read/[0-9]+/[0-9]+"::String) :: Bool

getPageUrl :: [Tag C.ByteString] -> [String]
getPageUrl tags = [""]

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
