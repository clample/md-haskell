module Html
       ( Html(..)
       , HtmlTag(..)
       , Header(..)
       , P(..)
       , A(..)
       , Ul(..)
       , Ol(..)
       , Li(..)
       , Img(..)
       ) where

data Html = Html [HtmlTag]
data HtmlTag = HtmlTag { render :: String }
data Header = Header { text :: String, ordinal :: Int }
data P = P String
data A = A { linkUrl :: String, linkText :: String }
data Ul= Ul [Li]
data Ol= Ol [Li]
data Li = Li String
data Img = Img { altText :: String, imgUrl :: String }
