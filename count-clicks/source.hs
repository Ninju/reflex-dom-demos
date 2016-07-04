import Reflex
import Reflex.Dom

-- Reflex.count does exactly the same thing! Uses "holdDyn"
countNumberOfEvents event = foldDyn (+) 0 $ tagDyn (constDyn 1) event

main = mainWidget $ do
  el "h1" $ text "Count clicks"
  el "div" $ do
    clickEvent <- button "Click me" -- clickEvent :: Event t ()
    numberOfTimesClicked <- count clickEvent
    el "p" $ do
      text "Clicked: " 
      dynText =<< mapDyn show numberOfTimesClicked
      text " times."
