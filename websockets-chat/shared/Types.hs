{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Shared.Types where
import Control.Lens
import Data.Aeson
import GHC.Generics

data ChatMessage = ChatMessage { _authorName :: String, _messageText :: String } deriving (Show, Generic)

makeLenses ''ChatMessage

instance ToJSON ChatMessage
instance FromJSON ChatMessage
