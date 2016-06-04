module Html where

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
