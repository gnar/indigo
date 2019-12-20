module Indigo.Render (
    renderListPages
  , renderViewPage
  , renderEditPage
  , renderMissingPage
  , renderListTags
  , renderGetTag
) where

import Control.Monad (forM_)
import Control.Lens ((^.))
import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc (ReaderOptions(..), WriterOptions(..), readMarkdown, writeHtml5, githubMarkdownExtensions, runPure, def)

import qualified Indigo.Api as Api
import Indigo.Page
import Indigo.WikiEnv
import Indigo.WikiTag

renderPageLink :: WikiEnv -> T.Text -> H.Html
renderPageLink env name = H.a ! A.href (H.toValue $ pageUrl env name) $ H.toHtml name

renderTagBadge :: WikiEnv -> T.Text -> H.Html
renderTagBadge env tag = H.a ! A.href (H.toValue $ tagUrl env tag) $ badge
  where
    badge = H.span ! A.class_ "badge badge-info" $ H.toHtml ("#" <> tag)

renderTemplate :: WikiEnv -> T.Text -> H.Html -> H.Html -> H.Html
renderTemplate env title menus contents =
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
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href (H.toValue $ pageUrl env (env ^. mainPage)) $ "Main"
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href (H.toValue $ pagesUrl env) $ "Pages"
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href (H.toValue $ tagsUrl env) $ "Tags"
              menus
            H.form ! A.class_ "form-inline my-2 my-lg-0" $ do
              H.input ! A.class_ "form-control mr-sm-2" ! A.type_ "search" ! A.placeholder "Search"
              H.button ! A.class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "Search"
      H.div ! A.class_ "container" $ do
        contents
        H.script ! A.src (H.toValue (staticLink "/static/jquery.min.js")) $ pure ()
        H.script ! A.src (H.toValue (staticLink "/static/bootstrap.min.js")) $ pure ()
  where
    staticLink path = (env ^. host) <> path

renderPageTemplate :: WikiEnv -> T.Text -> H.Html -> H.Html
renderPageTemplate env title contents = renderTemplate env title thisPageMenu contents
  where
    thisPageMenu = do
      H.li ! A.class_ "nav-item dropdown" $ do
        H.a ! A.class_ "nav-link dropdown-toggle" ! A.href "#" ! A.id "navbarDropdown" ! H.dataAttribute "toggle" "dropdown" $ "This page"
        H.div ! A.class_ "dropdown-menu" $ do
          H.a ! A.class_ "dropdown-item" ! A.href "?action=view" $ "View"
          H.a ! A.class_ "dropdown-item" ! A.href "?action=edit" $ "Edit"
          H.a ! A.class_ "dropdown-item" ! A.href "?action=delete" $ "Delete"

renderTagTemplate :: WikiEnv -> T.Text -> H.Html -> H.Html
renderTagTemplate env title contents = renderTemplate env title mempty contents

pageHtml :: WikiEnv -> Doc -> H.Html
pageHtml env page =
  let
      rdOpts = def { readerExtensions = githubMarkdownExtensions } :: ReaderOptions
      wrOpts = def :: WriterOptions
      text' = processWikiText env (page ^. text)
      res = runPure $ readMarkdown rdOpts text' >>= writeHtml5 wrOpts
  in case res of
      Left err -> undefined
      Right doc -> doc

renderListPages :: WikiEnv -> [T.Text] -> H.Html
renderListPages env names =
  renderTagTemplate env "Tags" $ do
    H.h1 "Pages"
    H.ul $ do
      forM_ names $ \name -> do
        H.li $ renderPageLink env name

renderViewPage :: WikiEnv -> Doc -> H.Html
renderViewPage env page =
  renderPageTemplate env (page ^. meta. name) $ do
    H.p $ forM_ (page ^. meta . tags) $ \tag -> do
      renderTagBadge env tag
      H.preEscapedText "&nbsp;"
    H.p $
      pageHtml env page

renderEditPage :: WikiEnv -> Doc -> H.Html
renderEditPage env page =
  renderPageTemplate env (page ^. meta . name) $ do
    H.h1 (H.toHtml (page ^. meta . name))
    H.form ! A.action "#" ! A.method "post" $ do
      H.input ! A.type_ "hidden" ! A.name "name" ! A.value (H.toValue $ page ^. meta . name)
      H.div ! A.class_ "form-group" $ do
        "Tags"
        let tagsText = T.intercalate "," (page ^. meta . tags)
        H.textarea ! A.name "tags" ! A.class_ "form-control" $ H.toHtml tagsText
      H.div ! A.class_ "form-group" $ do
        "Document"
        H.textarea ! A.name "text" ! A.class_ "form-control" ! A.rows "25" $ H.toHtml (page ^. text)
      H.div ! A.class_ "form-group" $
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"

renderMissingPage :: WikiEnv -> DocName -> H.Html
renderMissingPage env name =
  renderPageTemplate env name $ do
    H.h1 (H.toHtml name)
    H.p $ do
      H.toHtml ("Document " <> name <> " does not exist. ")
      H.a ! A.href "?action=create" $ "Create it?"

renderGetTag :: WikiEnv -> T.Text -> [DocMeta] -> H.Html
renderGetTag env tag metas =
  renderTagTemplate env tag $ do
    H.h1 (H.toHtml $ "#" <> tag)
    H.ul $ do
      forM_ metas $ \meta -> do
        H.li $ renderPageLink env (meta ^. name)

renderListTags :: WikiEnv -> [T.Text] -> H.Html
renderListTags env tags =
  renderTagTemplate env "Tags" $ do
    H.h1 "Tags"
    H.ul $ do
      forM_ tags $ \tag -> do
        H.li $ renderTagBadge env tag
