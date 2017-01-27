
module Web.Hen.Html where

import Lucid hiding (Html)
import Data.Monoid
import Data.Time.Calendar (toGregorian)

import Web.Hen.Config
import Web.Hen.Types


-- | A page template
template :: Config -> Html -> [Html] -> Html
template cfg nav posts = do
  doctype_
  html_ ([lang_ $ cfgLang cfg] ++ [dir_ "rtl" | cfgRtl cfg]) $ do
    head_ $ do
      meta_ $ [charset_ "utf-8"]

      title_ (toHtml $ cfgTitle cfg)

      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

      link_ [rel_ "stylesheet", type_ "text/css", href_ "css/normalize.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "css/style.css"]

    body_ $ do
      div_ [class_ "container"] $ do
        div_ [id_ "title"] $ do
          h1_ $ a_ [ href_ "index.html" ] $ toHtml (cfgTitle cfg)
          p_  $ toHtml (cfgDesc cfg)

        div_ [class_ "wrapper"] $ do

          div_ [class_ "content"] $ do
            mapM_ (div_ [class_ "post"]) posts

          aside_ [class_ "side"] nav


postHtml :: Post -> Html
postHtml Post{..} = do
  h2_ $ a_ [href_ $ postRoute <> ".html"] $
    toHtml postTitle
  h5_ $ date postDate

  postBody

 where
   date (toGregorian -> (yyyy,mm,dd)) =
     toHtml $ show dd <> "/" <> show mm <> "/" <> show yyyy

