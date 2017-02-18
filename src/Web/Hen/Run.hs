-- | Run the program

{-# LANGUAGE TemplateHaskell #-}

module Web.Hen.Run
  ( run
  )
where

import Control.Monad
import Data.List (sortOn)
import qualified Data.ByteString.Lazy as BSL (writeFile)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import Shelly (cp_r, fromText, shelly)
import Lucid (renderBS)

import Web.Hen.Config (parseArgs)
import Web.Hen.Html (template, postHtml)
import Web.Hen.Reader (readSite)
import Web.Hen.Types

run :: IO ()
run = do
  (cfg, override) <- parseArgs
  site <- readSite
  mkOutputDir override
  let index = template cfg (siteSide site) (map postHtml (reverse . sortOn postDate $ sitePosts site))
  BSL.writeFile ("output" </> "index.html") (renderBS index)

  forM_ (sitePosts site) $ \p -> do
    let pHtml = template cfg (siteSide site) [postHtml p]
    BSL.writeFile ("output" </> (T.unpack (postRoute p) ++ ".html")) (renderBS pHtml)

mkOutputDir :: Bool -> IO ()
mkOutputDir override = do
  when override $ do
    doesDirectoryExist "output" >>=
      (`when` removeDirectoryRecursive "output")
  
  createDirectory "output"
  shelly $ cp_r (fromText $ T.pack ("_site" </> "static" </> ".")) (fromText "output")
