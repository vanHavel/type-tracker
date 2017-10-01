module DOMUtil where

import Control.Monad.Eff (Eff, foreachE)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Node (childNodes, removeChild)
import DOM.Node.NodeList (item, length)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Document, ElementId(..), Node, NodeList, documentToNonElementParentNode, elementToNode)
import Data.Array (foldM, range)
import Data.Foldable (foldl)
import Prelude (Unit, bind, discard, map, pure, unit, ($), (-), (<$>), (<>), (>>=))
import Util (unsafeFromMaybe)

-- get current html document as dom document
getDocument :: forall e. Eff (dom :: DOM | e) Document
getDocument = do
  win <- window
  htmldoc <- document win
  pure $ htmlDocumentToDocument htmldoc

-- get element by id as dom node
elementById :: String -> Document -> forall e. Eff (dom :: DOM | e) Node
elementById id doc = elementToNode <$> unsafeFromMaybe <$> getElementById (ElementId id) (documentToNonElementParentNode doc)

-- turn NodeList to Array
nodeListToArray :: NodeList -> forall e. Eff (dom :: DOM | e) (Array Node)
nodeListToArray nl = do
  l <- length nl
  foldM (\arr i -> do
    node <- item i nl
    pure (arr <> [unsafeFromMaybe node]))
      [] (range 0 (l - 1))

-- delete all children of a node
deleteChildren :: Node -> forall e. Eff (dom :: DOM | e) Unit
deleteChildren parent = do
  children <- childNodes parent
  childarray <- nodeListToArray children
  foreachE childarray \child -> do
    _ <- removeChild child parent
    pure unit
