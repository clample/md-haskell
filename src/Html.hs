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

type Html = [HtmlTag]
data HtmlTag = HtmlTag { render :: String } deriving (Show)
data Header = Header { text :: String, ordinal :: Int } deriving (Show)
data P = P String deriving (Show)
data A = A { linkUrl :: String, linkText :: String } deriving (Show)
data Ul= Ul [Li] deriving (Show)
data Ol= Ol [Li] deriving (Show)
data Li = Li String deriving (Show)
data Img = Img { altText :: String, imgUrl :: String } deriving (Show)
