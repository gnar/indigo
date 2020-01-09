module Indigo.Render (
    renderListPages
  , renderViewPage
  , renderEditPage
  , renderListTags
  , renderGetTag
) where

import Control.Monad (forM_)
import Control.Lens ((^.))
import qualified Data.Text as T

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as A

import Text.Pandoc
  ( ReaderOptions(..)
  , WriterOptions(..)
  , Extension(..)
  , githubMarkdownExtensions
  , extensionsFromList
  , readMarkdown
  , writeHtml5
  , runPure
  , def
  , HTMLMathMethod(MathJax)
  )

import qualified Indigo.Api as Api
import Indigo.Page
import Indigo.Environment
import Data.Foldable (for_)

import qualified Text.Pandoc as P
import Network.URI (URI)

instance H.ToValue URI where
  toValue x = H.stringValue (show x)

renderPageLink :: Environment -> T.Text -> H.Html
renderPageLink env name = H.a ! A.href (H.toValue $ buildURI env $ Api.buildPageLink name Nothing) $ H.toHtml name

renderTagBadge :: Environment -> T.Text -> H.Html
renderTagBadge env tag = H.a ! A.href (H.toValue $ buildURI env $ Api.buildTagLink tag) $ badge
  where
    badge = H.span ! A.class_ "badge badge-info" $ H.toHtml ("#" <> tag)

renderTemplate :: Environment -> T.Text -> H.Html -> H.Html -> H.Html
renderTemplate env title menus contents =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.link ! A.rel "stylesheet" ! A.href (H.toValue (staticLink env "bootstrap.min.css"))
      H.link ! A.rel "stylesheet" ! A.href (H.toValue (staticLink env "user.css"))
      H.link ! A.rel "stylesheet" ! A.href "https://use.fontawesome.com/releases/v5.7.0/css/all.css" -- ! A.integrity "sha384-lZN37f5QGtY3VHgisS14W3ExzMWZxybE1SJSEsQp9S+oqd12jhcu+A56Ebc1zFSJ" ! A.crossorigin "anonymous"
      H.title $ H.toHtml title
    H.body $ do
      H.header $ do
        H.nav ! A.class_ "navbar navbar-expand-lg navbar-light bg-light" $ do
          H.div ! A.class_ "collapse navbar-collapse" ! A.id "navbarSupportedContent" $ do
            H.ul ! A.class_ "navbar-nav mr-auto" $ do
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href (H.toValue $ buildURI env $ Api.buildPageLink (env ^. envMainPage) Nothing) $ "Main"
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href (H.toValue $ buildURI env $ Api.buildRootLink) $ "Documents"
              H.li ! A.class_ "nav-item" $ H.a ! A.class_ "nav-link" ! A.href (H.toValue $ buildURI env $ Api.buildTagsLink) $ "Tags"
              menus
            H.form ! A.class_ "form-inline my-2 my-lg-0" $ do
              H.input ! A.class_ "form-control mr-sm-2" ! A.type_ "search" ! A.placeholder "Search"
              H.button ! A.class_ "btn btn-outline-success my-2 my-sm-0" ! A.type_ "submit" $ "Search"
      H.div ! A.class_ "container" $ do
        contents
        H.script ! A.src (H.toValue (staticLink env "jquery.min.js")) $ mempty
        H.script ! A.src (H.toValue (staticLink env "bootstrap.min.js")) $ mempty
        H.script ! A.id "MathJax-script" ! A.async "" ! A.src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" $ mempty

renderPageTemplate :: Environment -> T.Text -> Name -> H.Html -> H.Html
renderPageTemplate env title name contents = renderTemplate env title thisPageMenu contents
  where
    thisPageMenu = 
      H.li ! A.class_ "nav-item dropdown" $ do
        H.a ! A.class_ "nav-link dropdown-toggle" ! A.href "#" ! A.id "navbarDropdown" ! H.dataAttribute "toggle" "dropdown" $ "Actions"
        H.div ! A.class_ "dropdown-menu" $ do
          H.a ! A.class_ "dropdown-item" ! A.href (H.toValue $ buildURI env $ Api.buildPageLink name (Just Api.PageView)) $ "View"
          H.a ! A.class_ "dropdown-item" ! A.href (H.toValue $ buildURI env $ Api.buildPageLink name (Just Api.PageEdit)) $ "Edit"
          H.a ! A.class_ "dropdown-item" ! A.href (H.toValue $ buildURI env $ Api.buildPageLink name (Just Api.PageDelete)) $ "Delete"
          H.a ! A.class_ "dropdown-item" ! A.href (H.toValue $ buildURI env $ Api.buildFileLink (T.unpack name <> ".md")) $ "Download"

renderTagTemplate :: Environment -> T.Text -> H.Html -> H.Html
renderTagTemplate env title contents = renderTemplate env title mempty contents

pageHtml :: P.Pandoc -> H.Html
pageHtml pandoc =
  let
    wrOpts = def { writerHTMLMathMethod = MathJax "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" }
  in case runPure $ writeHtml5 wrOpts pandoc of
    Left err -> undefined
    Right doc -> doc

renderListPages :: Environment -> [Page] -> H.Html
renderListPages env pages =
  renderTagTemplate env "Tags" $ do
    H.h1 "Pages"
    H.ul $
      for_ pages $ \page ->
        H.li $ renderPageLink env (page ^. name)

renderViewPage :: Environment -> (Page, P.Pandoc) -> H.Html
renderViewPage env (page, pandoc) =
  renderPageTemplate env (page ^. name) (page ^. name) $ do
    H.p $ for_ (page ^. tags) $ \tag -> do
      renderTagBadge env tag
      H.preEscapedText "&nbsp;"
    H.p $
      pageHtml pandoc

renderEditPage :: Environment -> (Page, T.Text) -> H.Html
renderEditPage env (page, text) =
  renderPageTemplate env (page ^. name) (page ^. name) $ do
    H.h1 (H.toHtml ("Edit: " <> page ^. name))
    H.form ! A.action "#" ! A.method "post" $ do
      H.input ! A.type_ "hidden" ! A.name "name" ! A.value (H.toValue $ page ^. name)
      H.div ! A.class_ "form-group" $
        H.textarea ! A.name "text" ! A.class_ "form-control" ! A.rows "25" $ H.toHtml text
      H.div ! A.class_ "form-group" $
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"

renderGetTag :: Environment -> T.Text -> [Page] -> H.Html
renderGetTag env tag pages =
  renderTagTemplate env tag $ do
    H.h1 (H.toHtml $ "#" <> tag)
    H.ul $
      forM_ pages $ \page ->
        H.li $ renderPageLink env (page ^. name)

renderListTags :: Environment -> [T.Text] -> H.Html
renderListTags env tags =
  renderTagTemplate env "Tags" $ do
    H.h1 "Tags"
    H.ul $
      forM_ tags $ \tag ->
        H.li $ renderTagBadge env tag
