{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import qualified Data.Map as Map

type Task = String

data UserEvent a = Delete | Create a

applyUserOp :: UserEvent a -> Maybe a -> Maybe a
applyUserOp Delete = const Nothing
applyUserOp (Create a) = const (Just a)

initialTasks :: Map.Map Int Task
initialTasks = foldl (\curMap id -> Map.insert id ("Item #" ++ show id) curMap) Map.empty [1..5]

main = mainWidget app

updateWithMap alterations init = Map.foldlWithKey applyUserOperation init alterations
                                 where
                                 applyUserOperation m k op = Map.alter (applyUserOp op) k m

app = do
  rec userEvents   <- renderApp tasks
      newTaskEvent <- renderNewTaskForm
      newTaskIds   <- mapDyn (+6) =<< count newTaskEvent
      tasks        <- foldDyn updateWithMap
                              initialTasks
                              (appendEvents userEvents $ attachDynWith Map.singleton newTaskIds newTaskEvent)

  return ()

renderApp :: (Ord k, MonadWidget t m) => Dynamic t (Map.Map k Task) -> m (Event t (Map.Map k (UserEvent Task)))
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
