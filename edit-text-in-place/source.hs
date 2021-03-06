{-# LANGUAGE RecursiveDo #-}
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified GHCJS.DOM.JSFFI.Generated.HTMLInputElement as Input
import           Reflex
import           Reflex.Dom

placeholderName = "Click to edit the name"

main = mainWidget app

app = do
  el "h1" $ text "Edit item in place"
  el "p" $ text "Click on the name or the label to edit the name in-place."

  el "div" $ do
    (nameLabel, _) <- elAttr' "label" ("for" =: "name-input") $ el "strong" $ text "Name: "

    rec let nameClicked     = fmap (const True) $ appendEvents (domEvent Click nameElement) (domEvent Click nameLabel)
        let nameBlurred     = updated $ _textInput_hasFocus nameT
        let enterKeyPressed = fmap (const False) $ ffilter (== keycodeEnter) (_textInput_keypress nameT)

        let inEditingState = leftmost [enterKeyPressed, nameBlurred, nameClicked]

        performEvent_ $ fmap (const $ liftIO $ Input.select $ _textInput_element nameT) inEditingState

        nameTStyle <- dynStyleAttr "display" "none"   ("inline", "none") inEditingState
        nameAttrs  <- dynStyleAttr "display" "inline" ("inline", "none") $ fmap not inEditingState

        let defaultNameTAttrs = Map.fromList [("id", "name-input"), ("placeholder", "Enter a name")]

        nameTAttrs <- combineAttrs defaultNameTAttrs nameTStyle

        nameT <- textInput (def & attributes .~ nameTAttrs)

        (nameElement, _) <- elWith' "span" (def & elConfig_attributes .~ nameAttrs) $ do
          dynText =<< holdDyn placeholderName (updated $ _textInput_value nameT)

    return ()


dynStyleAttr :: (Reflex t, MonadHold t m) => String -> String -> (String, String) -> Event t Bool -> m (Dynamic t (Map.Map String String))
dynStyleAttr attrName initial (truthy, falsey) when = do
  currentAttr <- holdDyn initial (fmap (\t -> if t then truthy else falsey) when)
  mapDyn (\v -> Map.singleton "style" (attrName ++ ": " ++ v ++ ";")) currentAttr

combineAttrs :: (Reflex t, MonadHold t m) => Map.Map String String -> Dynamic t (Map.Map String String) -> m (Dynamic t (Map.Map String String))
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
