
module Web.Hen.Types where

import qualified Lucid as H (Html)
import Cheapskate (Options(..))
import Data.Text (Text)
import Data.Time.Calendar


type Html = H.Html ()

data Site = Site
  { siteSide  :: Html
  , sitePosts :: [Post]
  }
  deriving (Show)


data Post = Post
  { postRoute :: Text
  , postTitle :: Text
  , postDate  :: Day
  , postBody  :: Html
  }
  deriving (Show)


markdownOptions :: Options
markdownOptions =
  Options
    { sanitize = True
    , allowRawHtml = True
    , preserveHardBreaks = True
    , debug = False
    }
