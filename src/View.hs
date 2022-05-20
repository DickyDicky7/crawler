module View
  ( Html(..)
  , HTML(..)
  , homepageView
  ) where

import           Servant.HTML.Blaze             ( HTML )
import qualified Text.Hamlet                   as HTML
import           Text.Hamlet                    ( Html )
import           Universum
import qualified Universum.Unsafe              as Unsafe

waterCss :: Html
waterCss = [HTML.shamlet|
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/water.css@2/out/water.css">
|]

htmx :: Html
htmx = [HTML.shamlet|
  <script src="https://unpkg.com/htmx.org@1.7.0/dist/htmx.min.js" defer>
|]

template :: Maybe Html -> Html
template maybeBody = [HTML.shamlet|
  $doctype 5
  <html>
    <head>
      ^{waterCss}
    <body>
      $maybe body <- maybeBody
        ^{body}
      $nothing
|]

homepageView :: Html
homepageView = template $ Just [HTML.shamlet|
  <h2>Homepage
  <input type="text" value="Enter rss feed link here...">
  <button>Add
  $forall i <- l
    <div><input type="checkbox">#{i}
|]
 where
  l :: [Int]
  l = [1 .. 10]

