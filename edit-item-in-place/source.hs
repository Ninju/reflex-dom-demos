{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Reflex.Dom.Widget.Basic
import qualified Data.Map as Map
import GHCJS.DOM.Element
import Control.Monad.IO.Class

main = mainWidget app

app = do
  el "h1" $ text "Edit item in place"
  el "p" $ text "Click on the name to edit it in-place."

  el "div" $ do
    (nameLabel, _) <- elAttr' "label" ("for" =: "name-input") $ el "strong" $ text "Name: "

    rec let nameClicked     = fmap (const True) $ appendEvents (domEvent Click nameElement) (domEvent Click nameLabel)
        let nameBlurred     = updated $ _textInput_hasFocus nameT
        let enterKeyPressed = fmap (const False) $ ffilter (== keycodeEnter) (_textInput_keypress nameT)

        let inEditingState = leftmost [enterKeyPressed, nameBlurred, nameClicked]

        performEvent_ $ fmap (const $ liftIO $ focus $ _textInput_element nameT) inEditingState

        nameTStyle <- dynStyleAttr "display" inEditingState            "none"   ("inline", "none")
        nameAttrs  <- dynStyleAttr "display" (fmap not inEditingState) "inline" ("inline", "none")

        let defaultNameTAttrs = Map.fromList [("id", "name-input"), ("placeholder", "Enter a name")]

        nameTAttrs <- combineAttrs defaultNameTAttrs nameTStyle

        nameT <- textInput (def & attributes .~ nameTAttrs)

        (nameElement, _) <- elWith' "span" (def & elConfig_attributes .~ nameAttrs) $ do
          dynText =<< holdDyn placeholderName (updated $ _textInput_value nameT)

    return ()

dynStyleAttr attrName when initial (truthy, falsey) = do
  currentAttr <- holdDyn initial (fmap (\t -> if t then truthy else falsey) when)
  mapDyn (\v -> Map.singleton "style" (attrName ++ ": " ++ v ++ ";")) currentAttr

placeholderName = "Click to edit the name"

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
