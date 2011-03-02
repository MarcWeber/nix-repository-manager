module Types where

data Config = Config {
                cfgUrl :: String -- dirname of url for publishing
              , cfgUpload :: [String] -- command to publish
              , cfgRepoDir :: String
              }

data TaskType = TTFetch | TTPublish
  deriving (Eq, Ord, Show)


data DoWorkAction = DWUpdate | DWUpdateThenPublish | DWPublish
