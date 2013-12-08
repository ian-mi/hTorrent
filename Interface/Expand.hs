module Interface.Expand (expand) where

import Control.Applicative
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

expand :: Element -> [Element] -> UI Element
expand name body = do
    button <- UI.span
    let clickEvent = unionWith const (UI.click button) (UI.click name)
    expanded <-  accumB False (not <$ clickEvent)
    sink text (showButton <$> expanded) (element button)
    bodySpan <- sink children (setChildren body <$> expanded) UI.span
    row [element button, element name] #+ [element bodySpan]

showButton :: Bool -> String
showButton True = "-"
showButton False = "+"

setChildren :: [Element] -> Bool -> [Element]
setChildren es expanded = if expanded then es else []
