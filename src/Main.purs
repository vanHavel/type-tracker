module Main where

import Control.Monad.Eff (Eff, forE, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement)
import DOM.Node.Element (setAttribute, setClassName)
import DOM.Node.Node (appendChild, childNodes, removeChild, setTextContent)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode, elementToNode)
import DOMUtil (deleteChildren, elementById, getDocument)
import Data.Array (foldM, fromFoldable, length, replicate, zip)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Levels (levels)
import Parse (runParseTerm)
import Prelude (Unit, bind, discard, pure, unit, ($), (<$>), (<>))
import Util (unsafeFromMaybe)

-- levels solved
type Progress = Array Boolean

-- initially no level is solved
initialProgress :: Progress
initialProgress = [true, false]--replicate (length levels) false

-- get the Player's current progress (TODO)
getProgress :: forall e. Eff (e) Progress
getProgress = pure initialProgress

-- main entry point
main :: forall e. Eff (console :: CONSOLE, dom :: DOM | e) Unit
main = do
  fillLevelList

-- fill the nav div with the list of levels
fillLevelList :: forall e. Eff (dom :: DOM | e) Unit
fillLevelList = do
  progress <- getProgress
  let levelData = zip levels progress
  -- access and clear nav list
  doc <- getDocument
  levelList <- elementById "level-list" doc
  deleteChildren levelList
  -- recreate nav list using level and progress data
  foreachE levelData $ \(Tuple level done) -> do
    newLi <- elementToNode <$> createElement "li" doc
    newLink <- createElement "a" doc
    setTextContent (level.name) (elementToNode newLink)
    setAttribute "href" "#" newLink
    if done then setClassName "completed" newLink else pure unit
    _ <- appendChild (elementToNode newLink) newLi
    _ <- appendChild newLi levelList
    pure unit
