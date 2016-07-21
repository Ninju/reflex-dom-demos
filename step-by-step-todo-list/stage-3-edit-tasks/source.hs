{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Lens
import qualified Data.Map as Map
import           Reflex
import           Reflex.Dom

data Task = Task { _taskDescription :: String
                 , _taskCompleted :: Bool
                 }
                 deriving Show

makeLenses ''Task

data UserEvent a = Delete | Create a | Edit (a -> a)

applyUserOp :: UserEvent a -> Maybe a -> Maybe a
applyUserOp Delete = const Nothing
applyUserOp (Create a) = const (Just a)
applyUserOp (Edit f) = fmap f

newTask :: String -> Task
newTask taskDesc = Task { _taskDescription = taskDesc, _taskCompleted = False }

initialTasks :: Map.Map Int Task
initialTasks = foldl (\curMap id -> Map.insert id (newTask $ "Task #" ++ show id) curMap) Map.empty [1..5]

main = mainWidget app

updateWithMap = flip (Map.foldlWithKey applyUserOperation)
                where
                applyUserOperation accMap key op = Map.alter (applyUserOp op) key accMap

app = do
  rec userEvents   <- renderApp tasks
      newTaskEvent <- renderNewTaskForm
      newTaskIds   <- mapDyn (+6) =<< count newTaskEvent
      tasks        <- foldDyn updateWithMap
                              initialTasks
                              (appendEvents userEvents $ attachDynWith Map.singleton newTaskIds newTaskEvent)

      debugInfo tasks

  return ()

debugInfo tasks = do
  el "h2" $ do
    text "Debug info"

  el "p" $ do
    text "(Just to be sure the tasks are changing)"

  el "ul" $ do
    tasksAsList <- mapDyn Map.elems tasks
    simpleList tasksAsList $ \task -> el "li" $ display task

renderApp :: (Ord k, MonadWidget t m) => Dynamic t (Map.Map k Task) -> m (Event t (Map.Map k (UserEvent Task)))
renderApp dynTasks = do
  el "h1" $ text "Edit tasks"
  el "ul" $ do
    listViewWithKey dynTasks $ \k task -> do
      el "li" $ do
        dynText =<< mapDyn (view taskDescription) task
        completedCheckbox <- checkbox False def 
        deleteEvent <- button "Delete"

        let deleteEvents = fmap (const Delete) deleteEvent
        let editEvents = fmap (Edit . set taskCompleted) $ _checkbox_change completedCheckbox

        return $ leftmost [deleteEvents, editEvents]

renderNewTaskForm = do
  elAttr "label" ("for" =: "new-task-name") $ text "Task name: "
  t <- textInput (def & attributes .~ constDyn (Map.singleton "id" "new-task-name"))
  clickEvent <- button "Create task"
  dynCreates <- mapDyn (Create . newTask) (_textInput_value t)
  return $ tagDyn dynCreates clickEvent
