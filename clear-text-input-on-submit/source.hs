{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import Data.Align

main = mainWidget app

app = do
  el "h1" $ text "Clear text on submit"
  el "p" $ text "Press the button or the 'Return' key to submit."
  el "div" $ do
    rec let enterKeyPressed = ffilter (== keycodeEnter) (_textInput_keypress t)
        let formSubmitEvent = alignWith (const "") enterKeyPressed buttonClick
        t <- textInput (def & setValue .~ formSubmitEvent)
        buttonClick <- button "Submit text"

        el "p" $ do
          lastSubmittedText <- holdDyn "[Nothing submitted yet]" $ tag (current $ _textInput_value t) formSubmitEvent
          text "Last submitted: "
          dynText lastSubmittedText
        
    return ()
