module Indigo.Render (
    renderViewPage
  , renderEditPage
  , renderMissingPage
) where

import Control.Monad (forM_)
import Control.Lens ((^.))
import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc hiding (Reader(..))

import Indigo.Page
import Indigo.WikiEnv
import Indigo.WikiTag

pageHtml :: WikiEnv -> Page -> H.Html
pageHtml env page =
  let
      rdOpts = def { readerExtensions = githubMarkdownExtensions } :: ReaderOptions
      wrOpts = def :: WriterOptions
      text' = processWikiText env (page ^. text)
      res = runPure $ readMarkdown rdOpts text' >>= writeHtml5 wrOpts
  in case res of
      Left err -> undefined
      Right doc -> doc

renderPageTemplate :: WikiEnv -> T.Text -> H.Html -> H.Html
renderPageTemplate env title contents =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.meta ! A.name "description" ! A.content ""
      H.meta ! A.name "author" ! A.content ""
      H.link ! A.rel "stylesheet" ! A.href (H.toValue (staticLink "/static/bootstrap.min.css"))
      H.link ! A.rel "stylesheet" ! A.href (H.toValue (staticLink "/static/user.css"))
      H.title $ H.toHtml title
    H.body $ do
      H.header $ do
        H.nav ! A.class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
          H.div ! A.class_ "collapse navbar-collapse" ! A.id "navbarSupportedContent" $ do
            H.ul ! A.class_ "navbar-nav mr-auto" $ do
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href "Hauptseite" $ "Home"
              H.li ! A.class_ "nav-item dropdown" $ do
                H.a ! A.class_ "nav-link dropdown-toggle" ! A.href "#" ! A.id "navbarDropdown" ! H.dataAttribute "toggle" "dropdown" $ "This page"
                H.div ! A.class_ "dropdown-menu" $ do
                  H.a ! A.class_ "dropdown-item" ! A.href "?action=view" $ "View"
                  H.a ! A.class_ "dropdown-item" ! A.href "?action=edit" $ "Edit"
                  H.a ! A.class_ "dropdown-item" ! A.href "?action=delete" $ "Delete"
            H.form ! A.class_ "form-inline my-2 my-lg-0" $ do
              H.input ! A.class_ "form-control mr-sm-2" ! A.type_ "search" ! A.placeholder "Search"
              H.button ! A.class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "Search"
      H.div ! A.class_ "container" $ do
        contents
        H.script ! A.src (H.toValue (staticLink "/static/jquery.min.js")) $ pure ()
        H.script ! A.src (H.toValue (staticLink "/static/bootstrap.min.js")) $ pure ()
  where
    staticLink path = (env ^. host) <> path

renderViewPage :: WikiEnv -> Page -> H.Html
renderViewPage env page =
  renderPageTemplate env (page ^. name) $ do
    H.p $ forM_ (page ^. meta . tags) $ \tag -> do
      H.span ! A.class_ "badge badge-secondary" $ H.toHtml tag
      H.preEscapedText "&nbsp;"
    H.p $
      pageHtml env page

renderEditPage :: WikiEnv -> Page -> H.Html
renderEditPage env page =
  renderPageTemplate env (page ^. name) $ do
    H.h1 (H.toHtml (page ^. name))
    H.form ! A.action "#" ! A.method "post" $ do
      H.input ! A.type_ "hidden" ! A.name "name" ! A.value (H.toValue $ page ^. name)
      H.div ! A.class_ "form-group" $
        H.textarea ! A.name "text" ! A.class_ "form-control" ! A.rows "25" $ H.toHtml (page ^. text)
      H.div ! A.class_ "form-group" $
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"

renderMissingPage :: WikiEnv -> T.Text -> H.Html
renderMissingPage env name =
  renderPageTemplate env name $ do
      H.h1 (H.toHtml name)
      H.p $ do
        H.toHtml ("Page " <> name <> " does not exist. ")
        H.a ! A.href "?action=create" $ "Create it?"
