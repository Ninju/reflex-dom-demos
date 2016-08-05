{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))

import           Reflex
import           Reflex.Dom

import           Helpers.EditInPlace (editInPlaceWith)

data Task = Task { _taskDescription :: String
                 , _taskCompleted :: Bool
                 }
                 deriving Show

makeLenses ''Task

data Filter = All | Active | Completed deriving (Show, Eq)

data UserEvent a = Delete | Create a | Edit (a -> a) | Batch (a -> Bool) (UserEvent a)

class Validatable a where
  isValid :: a -> Bool

instance Validatable Task where
  isValid task = view taskDescription task /= ""

buttonAttr :: MonadWidget t m => Map String String -> m () -> m (Event t ())
buttonAttr attrs w = do
  (e, _) <- elAttr' "button" attrs $ w
  return $ domEvent Click e

applyUserOp :: Validatable a => UserEvent a -> Maybe a -> Maybe a
applyUserOp Delete = const Nothing
applyUserOp (Create a) = const (mfilter isValid (Just a))
applyUserOp (Edit f) = \t -> maybe t Just $ mfilter isValid $ fmap f t
applyUserOp (Batch f op) = \t -> maybe t (applyUserOp op . Just) $ mfilter f t

newTask :: String -> Task
newTask taskDesc = Task { _taskDescription = taskDesc, _taskCompleted = False }

initialTasks :: Map.Map Int Task
initialTasks = foldl (\curMap id -> Map.insert id (newTask $ "Task #" ++ show id) curMap) Map.empty [1..5]

satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter All       = const True
satisfiesFilter Completed = view taskCompleted
satisfiesFilter Active    = not . view taskCompleted

main = mainWidgetWithCss $(embedFile "index.css") app

updateWithMap = flip (Map.foldlWithKey applyUserOperation)
                where
                applyUserOperation accMap key op = Map.alter (applyUserOp op) key accMap

prepareBatchOp :: Reflex t => Dynamic t (Map.Map Int Task) -> Event t (UserEvent Task) -> Event t (Map.Map Int (UserEvent Task))
prepareBatchOp tasks = attachWith (\taskMap toggleVal -> Map.map (const toggleVal) taskMap) (current tasks)

app = do
  todoApp
  infoFooter

todoApp =
  elAttr "section" ("class" =: "todoapp") $ do
    rec newTaskEvent   <- header "todos"
        toggleAll      <- renderToggleAllButton
        userEvents     <- renderApp filteredTasks
        newTaskIds     <- mapDyn (+6) =<< count newTaskEvent
        tasks          <- foldDyn updateWithMap
                                  initialTasks
                                  $ mconcat [ userEvents
                                            , attachDynWith Map.singleton newTaskIds newTaskEvent
                                            , prepareBatchOp tasks toggleAll
                                            , prepareBatchOp tasks clearCompleted
                                            ]

        (activeFilter, clearCompleted) <- footer [All, Active, Completed] tasks
        filteredTasks                  <- combineDyn (Map.filter . satisfiesFilter) activeFilter tasks

    return ()

renderClearCompletedButton :: MonadWidget t m => m (Event t (UserEvent Task))
renderClearCompletedButton = do
  clearCompletedButton <- buttonAttr ("class" =: "clear-completed") $ text "Clear completed"
  return $ fmap (const $ Batch (view taskCompleted) Delete) clearCompletedButton

renderToggleAllButton :: MonadWidget t m => m (Event t (UserEvent Task))
renderToggleAllButton = do
  toggleButton <- button "Toggle All"
  return . fmap (Batch (const True) . Edit . set taskCompleted) . updated =<< toggle False toggleButton

renderApp :: (Ord k, MonadWidget t m) => Dynamic t (Map.Map k Task) -> m (Event t (Map.Map k (UserEvent Task)))
renderApp dynTasks = do
  el "ul" $ do
    listViewWithKey dynTasks $ \k task -> do
      el "li" $ do
        taskDescChange <- editInPlaceWith Dblclick =<< mapDyn (view taskDescription) task
        checkboxChange <- checkboxView (constDyn mempty) =<< mapDyn (view taskCompleted) task
        deleteEvent <- button "Delete"

        let deleteEvents                = fmap (const Delete) deleteEvent
        let toggleCompletedEvents       = fmap (Edit . set taskCompleted) checkboxChange
        let changeTaskDescriptionEvents = fmap (Edit . set taskDescription) taskDescChange

        return $ leftmost [deleteEvents, toggleCompletedEvents, changeTaskDescriptionEvents]

renderNewTaskForm = do
  rec let newValueEntered = textInputGetEnter newTaskInput
      newTaskInput <- textInput $ def & attributes .~ constDyn (  "class" =: "new-todo"
                                                               <> "placeholder" =: "What needs to be done?"
                                                               )
                                      & setValue .~ fmap (const "") newValueEntered

  dynCreates <- mapDyn (Create . newTask) (_textInput_value newTaskInput)
  return $ tag (current dynCreates) newValueEntered

renderFilters :: MonadWidget t m => [Filter] -> m (Event t Filter)
renderFilters filters =
  elAttr "ul" ("class" =: "filters") $ do
    filterEvents <- forM filters $ \filter -> do
      el "li" $ do
        (filterLink, _) <- elAttr' "a" ("href" =: "#/") $ text (show filter)
        return $ fmap (const filter) (domEvent Click filterLink)

    return $ leftmost filterEvents

infoFooter = elAttr "footer" ("class" =: "info") $ do
  el "p" $ text "Double-click to edit a todo"
  el "p" $ do
    text "Created by "
    elAttr "a" ("href" =: "http://www.github.com/Ninju") $ text "Alex Watt"
  el "p" $ do
    text "Part of "
    elAttr "a" ("href" =: "http://www.todomvc.com") $ text "TodoMVC"

header title =
  elAttr "header" ("class" =: "header") $ do
    el "h1" $ text title
    renderNewTaskForm

footer :: MonadWidget t m => [Filter] -> Dynamic t (Map Int Task) -> m (Dynamic t Filter, Event t (UserEvent Task))
footer filters tasks =
  elAttr "footer" ("class" =: "footer") $ do
    elAttr "span" ("class" =: "todo-count") $ do
      el "strong" $ text "0"
      text " items left."

    clearCompleted <- renderClearCompletedButton
    activeFilter <- holdDyn All =<< renderFilters filters

    return (activeFilter, clearCompleted)
