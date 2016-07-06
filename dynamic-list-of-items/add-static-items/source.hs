{-# LANGUAGE RecursiveDo, FlexibleContexts #-}
import Reflex
import Reflex.Dom
import qualified Data.Map as Map

renderStaticListOfItems :: MonadWidget t m => [String] -> m ()
renderStaticListOfItems items = el "ul" $ do
  mapM_ (el "li" . text) items

renderDynamicList items = do
  dynamicOfWidgets <- mapDyn renderStaticListOfItems items
  dyn dynamicOfWidgets

renderAddNewTaskForm = el "div" $ do
  elAttr "label" ("for" =: "task-name") $ text "Task name: "
  tInput <- textInput (def & attributes .~ constDyn (Map.singleton "id" "task-name"))
  clickEvent <- button "Add new task"
  return $ tagDyn (_textInput_value tInput) clickEvent

main = mainWidget $ do
  rec renderDynamicList currentTasks
      newTask <- renderAddNewTaskForm
      currentTasks <- foldDyn (:) [] newTask

  return ()
