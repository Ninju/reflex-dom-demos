{-# LANGUAGE TemplateHaskell #-}
import Reflex
import Reflex.Dom
import Data.FileEmbed
import qualified Data.Map as Map

main = mainWidgetWithCss $(embedFile "style.css") $ do
  elAttr "div" ("id" =: "page-wrap") $ do 
    el "header" $ do
      el "h1" $ text "External CSS Demo"

    el "ul" $ do
      el "li" $ text "use TemplateHaskell"
      el "li" $ text "import Data.FileEmbed"
      el "li" $ text "call 'mainWidgetWithCss'"
      el "li" $ do
        text "with '$(embedFile "
        el "em" $ text "yourCssFileName.css"
        text ")'"
      el "li" $ text "And the widget, just like 'mainWidget'"
