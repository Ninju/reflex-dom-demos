{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom
import qualified Data.Map as Map

type Task = String

initialTasks :: Map.Map Int Task
initialTasks = foldl (\curMap id -> Map.insert id ("Item #" ++ show id) curMap) Map.empty [1..5]

main = mainWidget app

updateWithMap alterations init =
  let updates = Map.toList alterations
  in
    foldl (\m (k,f) -> Map.updateWithKey (const f) k m) init updates

app = do
  rec userEvents <- renderApp tasks
      tasks      <- foldDyn updateWithMap initialTasks $ userEvents

  return ()

renderApp :: (Ord k, MonadWidget t m) => Dynamic t (Map.Map k Task) -> m (Event t (Map.Map k (Task -> Maybe Task)))
renderApp dynTasks = do
  el "h1" $ text "Delete static items"
  el "ul" $ do
    listViewWithKey dynTasks $ \k itemName -> do
      el "li" $ do
        dynText itemName
        deleteEvent <- button "Delete"
        return $ tagDyn (constDyn (const Nothing)) deleteEvent
