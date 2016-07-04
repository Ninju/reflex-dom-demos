import Reflex
import Reflex.Dom
import qualified Data.Map as Map

blueBoxStyle = "width: 100px; height: 100px; border: 1px solid blue;"
blueBgBoxStyle = "width: 100px; height: 100px; background-color: blue; border: 1px solid blue;"

boxStyleGivenToggle t = Map.singleton "style" (if t then blueBoxStyle else blueBgBoxStyle)

main = mainWidget $ do
  el "h1" $ text "Toggle button"
  el "p" $ text "Toggle the background color by clicking the button"
  clickEvent <- el "div" $ button "Click me"
  attrs <- toggle False clickEvent >>= mapDyn boxStyleGivenToggle
  emptyElWith "div" (def & elConfig_attributes .~ attrs)
