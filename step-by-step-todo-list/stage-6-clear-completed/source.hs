{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import           Control.Monad
import qualified Data.Map as Map
import           Reflex
import           Reflex.Dom

data Task = Task { _taskDescription :: String
                 , _taskCompleted :: Bool
                 }
                 deriving Show

data Filter = All | Active | Completed deriving (Show, Eq)

data UserEvent a = Delete | Create a | Edit (a -> a) | Batch (a -> Bool) (UserEvent a)

makeLenses ''Task

applyUserOp :: UserEvent a -> Maybe a -> Maybe a
applyUserOp Delete = const Nothing
applyUserOp (Create a) = const (Just a)
applyUserOp (Edit f) = fmap f
applyUserOp (Batch f op) = \t -> maybe t (applyUserOp op . Just) $ mfilter f t

newTask :: String -> Task
newTask taskDesc = Task { _taskDescription = taskDesc, _taskCompleted = False }

initialTasks :: Map.Map Int Task
initialTasks = foldl (\curMap id -> Map.insert id (newTask $ "Task #" ++ show id) curMap) Map.empty [1..5]

satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter All       = const True
satisfiesFilter Completed = view taskCompleted
satisfiesFilter Active    = not . view taskCompleted

main = mainWidget app

updateWithMap = flip (Map.foldlWithKey applyUserOperation)
                where
                applyUserOperation accMap key op = Map.alter (applyUserOp op) key accMap

prepareBatchOp :: Reflex t => Dynamic t (Map.Map Int Task) -> Event t (UserEvent Task) -> Event t (Map.Map Int (UserEvent Task))
prepareBatchOp tasks = attachWith (\taskMap toggleVal -> Map.map (const toggleVal) taskMap) (current tasks)

app = do
  rec filterChange   <- renderFilters [All, Active, Completed]
      toggleAll      <- renderToggleAllButton
      clearCompleted <- renderClearCompletedButton
      userEvents     <- renderApp filteredTasks
      newTaskEvent   <- renderNewTaskForm

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
  el "h1" $ text "Edit tasks"
  el "ul" $ do
    listViewWithKey dynTasks $ \k task -> do
      el "li" $ do
        dynText =<< mapDyn (view taskDescription) task
        checkboxChange <- checkboxView (constDyn mempty) =<< mapDyn (view taskCompleted) task
        deleteEvent <- button "Delete"

        let deleteEvents = fmap (const Delete) deleteEvent
        let editEvents = fmap (Edit . set taskCompleted) checkboxChange

        return $ leftmost [deleteEvents, editEvents]

renderNewTaskForm = do
  elAttr "label" ("for" =: "new-task-name") $ text "Task name: "
  t <- textInput (def & attributes .~ constDyn (Map.singleton "id" "new-task-name"))
  clickEvent <- button "Create task"
  dynCreates <- mapDyn (Create . newTask) (_textInput_value t)
  return $ tagDyn dynCreates clickEvent

renderFilters :: MonadWidget t m => [Filter] -> m (Event t Filter)
renderFilters filters =
  el "ul" $ do
    filterEvents <- forM filters $ \filter -> do
      filterClick <- button (show filter)
      return $ fmap (const filter) filterClick

    return $ leftmost filterEvents
