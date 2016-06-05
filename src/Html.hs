module Html where

{--
type Html = [HtmlTag]
data HtmlTag = HtmlTag { render :: String } deriving (Show)
data Header = Header { text :: String, ordinal :: Int } deriving (Show, Eq)
data P = P String deriving (Show, Eq)
data A = A { linkUrl :: String, linkText :: String } deriving (Show, Eq)
data Ul= Ul [Li] deriving (Show, Eq)
data Ol= Ol [Li] deriving (Show, Eq)
data Li = Li String deriving (Show, Eq)
data Img = Img { altText :: String, imgUrl :: String } deriving (Show, Eq)

headerToHtml :: Header -> HtmlTag
headerToHtml header = HtmlTag { render = "<h1>" ++ (text header) ++ "</h1>" }

pToHtml :: P -> HtmlTag
pToHtml (P string) = HtmlTag { render = "<p>" ++ string ++ "</p>"}

aToHtml :: A -> HtmlTag
aToHtml a = HtmlTag { render = "<a href='" ++ (linkUrl a) ++ "'>" ++ (linkText a) ++ "</a>" }

ulToHtml :: Ul -> HtmlTag
ulToHtml _ = HtmlTag { render = "render as html" }
--}

data Html = Content ContentTag | Node Tag [Html] deriving (Show, Eq)
data Tag = Tag { renderOpen :: String, renderClose :: String } deriving (Show, Eq)
data ContentTag = ContentTag { render :: String } deriving (Show, Eq)

hTag :: Int -> Tag
hTag ordinal = Tag {
                     renderOpen = "<h" ++ (show ordinal) ++ ">",
                     renderClose = "<h" ++ (show ordinal) ++ "/>"
                   }

aTag :: String -> Tag
aTag linkUrl = Tag {
                     renderOpen = "<a href=\"" ++ linkUrl ++ "\">",
                     renderClose = "</a>"
                   }

imgContentTag :: String -> String -> ContentTag
imgContentTag altText imgUrl = ContentTag { render = "<img src=\"" ++ imgUrl ++ "\" alt=\"" ++ altText ++ "\">" }

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
