module Html where

data Html = Content {render :: String} | Node Tag [Html] deriving (Show, Eq)
data Tag = Tag { renderOpen :: String, renderClose :: String } deriving (Show, Eq)

renderHtml :: Html -> String
renderHtml (Content {render = content }) = content
renderHtml (Node (Tag {renderOpen = open, renderClose = close}) htmlArr) =
  open ++ (renderHtmlArray htmlArr) ++ close
  where renderHtmlArray (html:htmlArr) = (renderHtml html) ++ (renderHtmlArray htmlArr)
        renderHtmlArray [] = ""

hTag :: Int -> Tag
hTag ordinal = Tag {
                     renderOpen = "<h" ++ (show ordinal) ++ ">",
                     renderClose = "</h" ++ (show ordinal) ++ ">"
                   }

aTag :: String -> Tag
aTag linkUrl = Tag {
                     renderOpen = "<a href=\"" ++ linkUrl ++ "\">",
                     renderClose = "</a>"
                   }

imgContent :: String -> String -> Html
imgContent altText imgUrl = Content { render = "<img src=\"" ++ imgUrl ++ "\" alt=\"" ++ altText ++ "\">" }

liTag :: Tag
liTag = Tag {
              renderOpen = "<li>",
              renderClose = "</li>"
            }

pTag :: Tag
pTag = Tag {
             renderOpen = "<p>",
             renderClose = "</li>"
           }

ulTag :: Tag
ulTag = Tag {
              renderOpen = "<ul>",
              renderClose = "</ul>"
            }
