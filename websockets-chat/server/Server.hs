{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Server.Server where
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text as T
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           Shared.Types

nicknames = cycle ["Dave", "Alex", "Jack", "Donald", "Hilary", "Bernie", "Jill", "Terry", "Richard", "Ryan", "Sam", "Colin", "Bryce", "Damien"]

data AppState = AppState { _messages :: [ChatMessage], _activeConnections :: [Connection] }

makeLenses ''AppState

initialAppState = AppState { _messages = [], _activeConnections = [] }

instance Show AppState where
  show appState = "App has "
                  ++ (show $ length $ view activeConnections appState)
                  ++ " connections and " 
                  ++ (show $ length $ view messages appState) 
                  ++ " messages."

main = do
  putStrLn "Starting server..."
  putStrLn "Listening on port 3000"

  appState <- newMVar initialAppState

  run 3000 $ websocketsOr defaultConnectionOptions (wsApp appState) backupApp

wsApp :: MVar AppState -> ServerApp
wsApp appState pendingConn = do
  conn <- acceptRequest pendingConn
  registerConnection appState conn

  appState' <- tryReadMVar appState

  let nickname = nicknames !! (fromMaybe 0 (appState' >>= return . length . view activeConnections))

  forkPingThread conn 10

  forever $ do
    listenForMessages appState conn nickname

backupApp :: Application
backupApp request respond = respond $ case rawPathInfo request of
    "/" -> indexPage
    _   -> notFound

notFound :: Network.Wai.Response
notFound = responseLBS
  status404
  []
  "Nothing was found here."

indexPage :: Network.Wai.Response
indexPage = responseLBS
  status200 
  [("Content-Type", "text/plain")]
  "Please connect via websockets to use the chat server."

listenForMessages :: MVar AppState -> Connection -> String -> IO ()
listenForMessages appState conn nickname = do
  raw <- receiveData conn

  case decode raw :: Maybe ChatMessage of
    Just newChatMsg -> handleNewChatMessage appState (set authorName nickname newChatMsg)
    Nothing         -> putStrLn $ "We could not decode the raw message."

handleNewChatMessage :: MVar AppState -> ChatMessage -> IO ()
handleNewChatMessage appState chatMessage = do
  putStrLn $ "New chat message received: " ++ show chatMessage

  storeMessage appState chatMessage
  logAppState appState

  mayCurrentAppState <- tryReadMVar appState

  case mayCurrentAppState of
    Nothing -> return ()
    Just state -> do
      forM_ (_activeConnections state) $ \conn -> sendBinaryData conn (encode chatMessage)

logAppState :: MVar AppState -> IO ()
logAppState appState = do
  mayCurrentAppState <- tryReadMVar appState

  case mayCurrentAppState of
    Nothing    -> putStrLn "There is no app state."
    Just state -> putStrLn $ "App State: " ++ show state

registerConnection :: MVar AppState -> Connection -> IO ()
registerConnection appState conn = modifyMVar_ appState (return . over activeConnections (conn:))

storeMessage :: MVar AppState -> ChatMessage -> IO ()
storeMessage appState msg = modifyMVar_ appState (return . over messages (msg:))
