module Interface.Expand (expand) where

import Common

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

expand :: Element -> [Element] -> UI Element
expand name body = do
    button <- UI.span
    let clickEvent = unionWith const (UI.click button) (UI.click name)
    expanded <-  accumB False (not <$ clickEvent)
    sink text (showButton <$> expanded) (UI.element button)
    bodySpan <- sink UI.children (setChildren body <$> expanded) UI.span
    row [UI.element button, UI.element name] #+ [UI.element bodySpan]

showButton :: Bool -> String
showButton True = "-"
showButton False = "+"

setChildren :: [Element] -> Bool -> [Element]
setChildren es expanded = if expanded then es else []
