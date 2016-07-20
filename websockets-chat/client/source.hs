{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
import           Control.Lens
import           Control.Monad (void)
import           Data.Aeson
import           Data.FileEmbed
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import           Reflex
import           Reflex.Dom
import           Shared.Types
import           Data.ByteString.Lazy as LBS hiding (map)

wsUri = "ws://localhost:3000"

data UserEvent a = Create a deriving Show

initialMessages :: Map.Map Integer ChatMessage
initialMessages = Map.empty

applyEvent :: UserEvent ChatMessage -> Map.Map Integer ChatMessage -> Map.Map Integer ChatMessage
applyEvent (Create msg) map =
  case Map.maxViewWithKey map of
    Nothing -> Map.singleton (toEnum 0) msg
    Just ((k, _), _) -> Map.insert (succ k) msg map

stripUserEvent (Create msg) = msg

buildMessage :: UserEvent String -> UserEvent ChatMessage
buildMessage (Create msgText) = Create $ ChatMessage { _authorName = "Auto-assigned by server", _messageText = msgText }

main = mainWidgetWithCss $(embedFile "client/style.css") $ do
  rec wsocket <- webSocket wsUri $ def & webSocketConfig_send .~ (fmap ((:[]) . LBS.toStrict . encode . stripUserEvent) authoredUserEvents)
      let decodedMessages = fmap (decode . LBS.fromStrict) $ _webSocket_recv wsocket
      let successfullyDecodedMessages = ffilter isJust decodedMessages
      let receivedMessageEvents = fmap (Create . fromJust) successfullyDecodedMessages

      userEvent <- app messages

      let authoredUserEvents = fmap buildMessage userEvent

      messages <- foldDyn applyEvent initialMessages receivedMessageEvents

  return ()

app :: MonadWidget t m => Dynamic t (Map.Map Integer ChatMessage) -> m (Event t (UserEvent String))
app messages = do
  elAttr "div" ("id" =: "page-wrap") $ do
    el "h1" $ text "Reflex-DOM Chat App"

    el "p" $ do
      text "Using "
      elAttr "a" ("href" =: "https://github.com/reflex-frp/reflex-dom") $ text "reflex-dom" 
      text " for front-end and "
      elAttr "a" ("href" =: "https://hackage.haskell.org/package/wai-websockets") $ text "wai-websockets"
      text " on the backend with in-memory persistence."

    el "p" $ text "Chat usernames are automatically assigned from a pre-defined list."

    renderMessages messages

    (newMessageInput, newMessageEvent) <- renderNewMessageForm

    elAttr "div" ("class" =: "debug-info") $ do
      el "h2" $ text "Debug info"

      messageThatWasSent <- mapDyn ("Last sent message: " ++) =<< holdDyn "" newMessageEvent

      el "p" $ dynText messageThatWasSent

      el "p" $ do
        text "Message preview: "
        dynText newMessageInput

    return $ fmap Create newMessageEvent

renderMessages messages = do
  elAttr "ul" ("class" =: "message-list") $ do
    listViewWithKey messages $ \_ message ->
      el "li" $ do
        dynText =<< mapDyn (\msg -> view authorName msg ++ ": " ++ view messageText msg) message
        return never

renderNewMessageForm :: MonadWidget t m => m (Dynamic t String, Event t String)
renderNewMessageForm = el "div" $ do
  rec newChatMessageInput <- textInput (def & setValue .~ newMessageSent)
      sendEvent <- button "Send"
      let enterKeyPressed = fmap (const ()) $ ffilter (== keycodeEnter) (_textInput_keypress newChatMessageInput)
      let newMessageSent = fmap (const "") $ leftmost [enterKeyPressed, sendEvent]

  return $ (_textInput_value newChatMessageInput, tag (current $ _textInput_value newChatMessageInput) newMessageSent)
