{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import qualified Data.Map as Map

type Task = String

data CrudEvent a = Delete | Create a

initialTasks :: Map.Map Int Task
initialTasks = foldl (\curMap id -> Map.insert id ("Item #" ++ show id) curMap) Map.empty [1..5]

main = mainWidget app

updateWithMap alterations init =
  let updates = Map.toList alterations
  in
    foldl (uncurry . applyCrudOperation) init updates
  where
  applyCrudOperation m k (Delete)   = Map.updateWithKey (const . const Nothing) k m
  applyCrudOperation m k (Create a) = Map.alter (const (Just a)) k m

app = do
  rec userEvents   <- renderApp tasks
      newTaskEvent <- renderNewTaskForm
      newTaskIds   <- mapDyn (+6) =<< count newTaskEvent
      tasks        <- foldDyn updateWithMap
                              initialTasks
                              (appendEvents userEvents $ attachDynWith Map.singleton newTaskIds newTaskEvent)

  return ()

renderApp :: (Ord k, MonadWidget t m) => Dynamic t (Map.Map k Task) -> m (Event t (Map.Map k (CrudEvent Task)))
renderApp dynTasks = do
  el "h1" $ text "Add and delete static items"
  el "ul" $ do
    listViewWithKey dynTasks $ \k itemName -> do
      el "li" $ do
        dynText itemName
        deleteEvent <- button "Delete"
        return $ tagDyn (constDyn Delete) deleteEvent

renderNewTaskForm = do
  elAttr "label" ("for" =: "new-task-name") $ text "Task name: "
  t <- textInput (def & attributes .~ constDyn (Map.singleton "id" "new-task-name"))
  clickEvent <- button "Create task"
  dynCreates <- mapDyn Create (_textInput_value t)
  return $ tagDyn dynCreates clickEvent
