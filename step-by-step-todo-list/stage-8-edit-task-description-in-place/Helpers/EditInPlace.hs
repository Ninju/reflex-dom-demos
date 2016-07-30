{-# LANGUAGE RecursiveDo #-}
module Helpers.EditInPlace (editInPlace) where
import           Control.Monad.IO.Class
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified GHCJS.DOM.JSFFI.Generated.HTMLInputElement as Input
import           Reflex
import           Reflex.Dom

editInPlace :: MonadWidget t m => Dynamic t String -> m (Event t String)
editInPlace someDynText = do
  initialText <- sample (current someDynText)

  rec let textClicked     = fmap (const True) $ domEvent Click textElement
      let nameBlurred     = updated $ _textInput_hasFocus textInputT
      let enterKeyPressed = fmap (const False) $ ffilter (== keycodeEnter) (_textInput_keypress textInputT)

      let inEditingState = leftmost [enterKeyPressed, nameBlurred, textClicked]

      let defaultInputAttrs = Map.fromList [("id", "text-input")]

      textInputTStyle <- dynStyleAttr "display" "none"   ("inline", "none") inEditingState
      textAttrs       <- dynStyleAttr "display" "inline" ("inline", "none") $ fmap not inEditingState
      textInputTAttrs <- combineAttrs defaultInputAttrs textInputTStyle

      performEvent_ $ fmap (const $ liftIO $ Input.select $ _textInput_element textInputT) inEditingState

      -- text input should display exactly what is inside 'someDynText' except when editing
      textInputT <- textInput $ def & attributes .~ textInputTAttrs
                                    & textInputConfig_initialValue .~ initialText
                                    & textInputConfig_setValue .~ updated someDynText

      (textElement, _) <- elWith' "span" 
                                  (def & elConfig_attributes .~ textAttrs)
                                  $ dynText someDynText

  -- return the current value of the text input only when editing is finished
  -- because if validations fail, then we want roll-back to our previous valid value
  return $ tag (current $ _textInput_value textInputT) (ffilter (== False) inEditingState)

dynStyleAttr :: (Reflex t, MonadHold t m) => String -> String -> (String, String) -> Event t Bool -> m (Dynamic t (Map String String))
dynStyleAttr attrName initial (truthy, falsey) when = do
  currentAttr <- holdDyn initial (fmap (\t -> if t then truthy else falsey) when)
  mapDyn (\v -> Map.singleton "style" (attrName ++ ": " ++ v ++ ";")) currentAttr

combineAttrs :: (Reflex t, MonadHold t m) => Map String String -> Dynamic t (Map String String) -> m (Dynamic t (Map String String))
combineAttrs defAttrs dynAttrs =
  combineDyn
    (\defArgs extraArgs ->
        foldr (\(attrName, attrVal) curAttrs ->
            Map.alter (const $ Just attrVal)
                      attrName
                      curAttrs)
              defArgs
              (Map.toList extraArgs))
    (constDyn defAttrs)
    dynAttrs
