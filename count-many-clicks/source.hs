{-# LANGUAGE ScopedTypeVariables, RankNTypes, AllowAmbiguousTypes #-}
import Reflex
import Reflex.Dom
import Data.Monoid
import Control.Monad

main = mainWidget $ do
  el "h1" $ text "Count many clicks"
  el "table" $ do
    el "th" $ text "Button"
    el "th" $ text "Number of clicks"

    button1Click <- buttonRow "Button #1"
    button2Click <- buttonRow "Button #2"
    button3Click <- buttonRow "Button #3"

    el "tr" $ do
      el "td" $ do
        text "Total"

      el "td" $ do
        individualClickTotals <- mapM (mapDyn Sum <=< count) [button1Click, button2Click, button3Click]
        totalClicks <- mconcatDyn individualClickTotals
        display =<< mapDyn getSum totalClicks

buttonRow :: forall t m b. (MonadWidget t m, Num b, Show b) => String -> m (Event t ())
buttonRow buttonName = el "tr" $ do
  buttonClick <- el "td" $ do
    button buttonName

  nClicks <- count buttonClick
  el "td" $ display (nClicks :: Dynamic t b)

  return buttonClick
