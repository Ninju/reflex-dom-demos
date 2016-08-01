{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.FileEmbed
import qualified Data.Map as Map
import           Data.Monoid ((<>))

import           Reflex
import           Reflex.Dom

import           Helpers.EditInPlace (editInPlace)

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

header title =
  elAttr "header" ("class" =: "header") $ do
    el "h1" $ text title
    renderNewTaskForm

app = do
  elAttr "section" ("class" =: "todoapp") $ do
    rec newTaskEvent   <- header "todos"
        filterChange   <- renderFilters [All, Active, Completed]
        toggleAll      <- renderToggleAllButton
        clearCompleted <- renderClearCompletedButton
        userEvents     <- renderApp filteredTasks

        newTaskIds     <- mapDyn (+6) =<< count newTaskEvent
        tasks          <- foldDyn updateWithMap
                                  initialTasks
                                  $ mconcat [ userEvents
                                            , attachDynWith Map.singleton newTaskIds newTaskEvent
                                            , prepareBatchOp tasks toggleAll
                                            , prepareBatchOp tasks clearCompleted
                                            ]

        activeFilterDyn    <- holdDyn All filterChange
        filteredTasks      <- combineDyn (Map.filter . satisfiesFilter) activeFilterDyn tasks

    return ()

renderClearCompletedButton :: MonadWidget t m => m (Event t (UserEvent Task))
renderClearCompletedButton = do
  clearCompletedButton <- button "Clear Completed"
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
        taskDescChange <- editInPlace =<< mapDyn (view taskDescription) task
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
  el "ul" $ do
    filterEvents <- forM filters $ \filter -> do
      filterClick <- button (show filter)
      return $ fmap (const filter) filterClick

    return $ leftmost filterEvents
