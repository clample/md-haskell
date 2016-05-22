module MarkdownParser where

import Data.List
import Data.Int
import Html
import Data.Char (isSpace)
import ParseUtil


parseHeader :: Parse Header
parseHeader = ((length) <$> (parseWhile (\c -> c == '#'))) ==>
              \ordnl -> skipSpaces ==>&
              parseWhile (\c -> c /= '\n') ==>
              \txt -> identity (Header { text = txt, ordinal = ordnl })

parseLink :: Parse A
parseLink = skipBracket ==>&
            parseWhile (\c -> c /= ']') ==>
            \linkText -> skipBracket ==>&
            skipParenthesis ==>&
            parseWhile (\c -> c /= ')') ==>
            \linkUrl -> skipParenthesis ==>&
            identity (A { linkUrl = linkUrl, linkText = linkText })
            where skipBracket = parseChar
                  skipParenthesis = parseChar

parseImage :: Parse Img
parseImage = skipExclamation ==>&
             skipBracket ==>&
             parseWhile (\c -> c /= ']') ==>
             \altText -> skipBracket ==>&
             skipParenthesis ==>&
             parseWhile (\c -> c /= ')') ==>
             \imgUrl -> skipParenthesis ==>&
             identity (Img { altText = altText, imgUrl = imgUrl })
             where skipExclamation = parseChar
                   skipParenthesis = parseChar
                   skipBracket = parseChar

parseUnorderedList :: Parse Ul
parseUnorderedList = peekChar ==>
                     \maybeChar -> case maybeChar of
                                   Just '*' -> parseUnorderedListElement ==>
                                               \li -> parseUnorderedList ==>
                                               \(Ul ul) -> identity (Ul (li:ul))
                                   Just _ -> identity (Ul [])
                                   Nothing -> identity (Ul [])

parseUnorderedListElement :: Parse Li
parseUnorderedListElement = skipSpecialChar ==>&
                            skipSpace ==>&
                            parseWhile (\c -> c /= '\n') ==>
                            \content -> skipNewline ==>&
                            identity (Li content)
                            where skipSpecialChar = parseChar
                                  skipSpace = parseChar
                                  skipNewline = skipCharIfItExists

parseParagraph :: Parse P
parseParagraph = parseWhile (\c -> c /= '\n') ==>
                 \content -> skipCharIfItExists ==>&
                 peekChar ==>
                 \maybeChar -> case maybeChar of
                               Just '\n' -> skipCharIfItExists ==>&
                                            identity (P content)
                               Just _ -> parseParagraph ==>
                                         \(P moreContent) -> identity (P (content ++ moreContent))
                               Nothing -> identity (P content)
