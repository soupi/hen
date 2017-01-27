
module Web.Hen.Config
  ( Config(..)
  , parseArgs
  )
where

import Data.Maybe (fromMaybe)
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Configurator as C

parseArgs :: IO (Config, Bool)
parseArgs = do
  args <- execParser paramsParserInfo
  (, pOvrd args) <$>
    case (parseConfig <$> pFile args, pCfg args) of
      (_, Just cfg) ->
        pure cfg
      (Just runCfg, _) ->
        runCfg
      _ ->
        pure defaultConfig


-- | Parse configuration file
parseConfig :: FilePath -> IO Config
parseConfig cfgFile = do
  cfg  <- C.load [C.Required cfgFile]
  name <- C.require cfg "name"
  desc <- C.require cfg "description"
  lang <- fromMaybe (cfgLang defaultConfig)
      <$> C.lookup cfg "lang"
  rtl  <- fromMaybe (cfgRtl defaultConfig)
      <$> C.lookup cfg "rtl"

  pure (Config name desc lang rtl)

------------
-- Config --
------------

-- | Configuration
data Config = Config
  { cfgTitle :: T.Text -- ^Title of the website
  , cfgDesc  :: T.Text -- ^Description of the website
  , cfgLang  :: T.Text -- ^Website language
  , cfgRtl   :: Bool   -- ^Right to left
  }
  deriving (Show, Eq, Ord)

defaultConfig :: Config
defaultConfig = Config
  { cfgTitle = "Example"
  , cfgDesc  = "A hen example website"
  , cfgLang  = "en"
  , cfgRtl   = False
  }

paramsParserInfo :: ParserInfo Params
paramsParserInfo =
  info (helper <*> config) $
     fullDesc
  <> header "Hen - a static website generator"

data Params = Params
  { pCfg  :: Maybe Config
  , pOvrd :: Bool
  , pFile :: Maybe FilePath
  }


config :: Parser Params
config = Params
  <$> optional paramsConfig
  <*> override
  <*> optional file
  where
    override =
      switch
        (long "override"
         <> help "Whether to override the output folder"
        )
    file =
      strOption
      (long "config"
       <> short 'f'
       <> metavar "FILE"
       <> help "Path to configuration file"
      )

paramsConfig :: Parser Config
paramsConfig = Config
  <$> fmap T.pack ttl
  <*> fmap T.pack desc
  <*> fmap T.pack lang
  <*> rtl
  where
    ttl =
      strOption
        (long "title"
         <> short 't'
         <> metavar "NAME"
         <> help "Website title"
        )
    desc =
      strOption
        (long "description"
         <> short 'd'
         <> metavar "DESC"
         <> help "Website description"
        )
    lang =
      strOption
        (long "lang"
         <> short 'l'
         <> metavar "LANG"
         <> help "Website language"
         <> value (T.unpack $ cfgLang defaultConfig)
         <> showDefault
        )
    rtl =
      switch
        (long "rtl"
         <> help "Whether to use rtl direction"
        )
