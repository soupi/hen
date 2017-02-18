{- | Site file reader

This module is responsible for reading the website's
content files and create a Haskell representation of them

-}

module Web.Hen.Reader
  ( readSite
  )
where

import Cheapskate.Lucid
import Cheapskate (markdown)
import Data.Monoid
import Control.Monad
import Control.Arrow
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.Directory (listDirectory)
import System.FilePath
import Data.Time.Calendar

import Web.Hen.Types


readSite :: IO Site
readSite =
  Site
    <$> side
    <*> posts


side :: IO Html
side = do
  navFile <- TIO.readFile ("_site" </> "side.md")
  pure $ renderDoc $ markdown markdownOptions navFile

posts :: IO [Post]
posts = do
  dir <- listDirectory ("_site" </> "_posts")
  postsTexts <- zip dir <$> traverse (\f -> TIO.readFile $ "_site" </> "_posts" </> f) dir
  traverse
    (uncurry readPost)
    postsTexts

readPost :: FilePath -> Text -> IO Post
readPost f = do
  either
    (\e -> die ("Error in file: " <> f <> ": " <> T.unpack e))
    pure
  . parsePost

parsePost :: Text -> Either Text Post
parsePost =
    (\(m,t) -> ($ renderDoc $ markdown markdownOptions t) <$> parseMeta m)
  . second (T.unlines . drop 1)
  . break (=="---")
  . T.lines
  where
    parseMeta meta =
      Post
        <$> match "route" lt
        <*> match "title" lt
        <*> (parseDay =<< match "date" lt)
      where
        lt = map ((T.toLower *** T.drop 1) . T.break (=='=')) meta

match :: Text -> [(Text, a)] -> Either Text a
match key =
  maybe
    (Left $ "Could not find the metadata field: '" <> key <> "'.")
    Right
    . lookup key 

parseDay :: Text -> Either Text Day
parseDay t =
  maybe
    (Left "Invalid format for date. expecting format: YYYY-MM-DD")
    Right $
    case T.split (=='-') t of
      [yyyy,mm,dd] -> join $
        fromGregorianValid
          <$> readMaybe yyyy
          <*> readMaybe mm
          <*> readMaybe dd
      _ ->
        Nothing

readMaybe :: Read a => Text -> Maybe a
readMaybe t =
  case reads (T.unpack t) of
    [(r,[])] -> pure r
    _ -> Nothing
