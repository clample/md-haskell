module MarkdownParser where

import Data.List
import Data.Int
import Html
import Data.Char (isSpace)
import ParseUtil


parseHeader :: Parse Html
parseHeader = ((length) <$> (parseWhile (\c -> c == '#'))) ==>
              \ordinal -> skipSpaces ==>&
              parseWhile (\c -> c /= '\n') ==>
              \txt -> skipCharIfItExists ==>&
              identity (Node (hTag ordinal) ((Content { render = txt }):[]))

parseLink :: Parse Html
parseLink = skipBracket ==>&
            parseWhile (\c -> c /= ']') ==>
            \linkText -> skipBracket ==>&
            skipParenthesis ==>&
            parseWhile (\c -> c /= ')') ==>
            \linkUrl -> skipParenthesis ==>&
            identity (Node (aTag linkUrl) ((Content { render = linkText }):[]))
            where skipBracket = parseChar
                  skipParenthesis = parseChar

parseImage :: Parse Html
parseImage = skipExclamation ==>&
             skipBracket ==>&
             parseWhile (\c -> c /= ']') ==>
             \altText -> skipBracket ==>&
             skipParenthesis ==>&
             parseWhile (\c -> c /= ')') ==>
             \imgUrl -> skipParenthesis ==>&
             identity ((imgContent altText imgUrl))
             where skipExclamation = parseChar
                   skipParenthesis = parseChar
                   skipBracket = parseChar

parseUnorderedList :: Parse Html
parseUnorderedList = peekChar ==>
                     \maybeChar -> case maybeChar of
                                   Just '*' -> parseUnorderedListElement ==>
                                               \li -> parseUnorderedList ==>
                                               \(Node tag ul) -> identity (Node ulTag (li:ul))
                                   Just _ -> identity (Node ulTag [])
                                   Nothing -> identity (Node ulTag [])
                                 

parseUnorderedListElement :: Parse Html
parseUnorderedListElement = skipSpecialChar ==>&
                            skipSpace ==>&
                            parseWhile (\c -> c /= '\n') ==>
                            \content -> skipNewline ==>&
                            identity (Node liTag ((Content {render = content}):[]) )
                            where skipSpecialChar = parseChar
                                  skipSpace = parseChar
                                  skipNewline = skipCharIfItExists

parseParagraph :: Parse Html
parseParagraph = parseWhile (\c -> c /= '\n') ==>
                 \content -> skipCharIfItExists ==>&
                 peekChar ==>
                 \maybeChar -> case maybeChar of
                               Just '\n' -> skipCharIfItExists ==>&
                                            identity (Node pTag ((Content {render = content }):[]))
                               Just _ -> parseParagraph ==>
                                         \(Node tag ((contentTag):contentTags)) -> identity (Node pTag ((Content { render = (content ++ "<br>" ++ (render contentTag)) }):[]))
                               Nothing -> identity (Node pTag ((Content { render = content }):[]))

parseMarkdown :: Parse Html
parseMarkdown = peekChar ==>
                \maybeChar -> case maybeChar of
                              Nothing -> identity (Content {render = ""})
                              Just char -> dispatchToParser char ==>
                                           \htmlBlock -> parseMarkdown ==>
                                           \(Node tag html) -> identity (Node (Tag {renderOpen="", renderClose=""}) (htmlBlock:html))
                             

dispatchToParser :: Char -> Parse Html
dispatchToParser '#' = parseHeader ==>
                       \headerTag -> identity (headerTag)
dispatchToParser _ = parseParagraph ==>
                     \paragraphTag -> identity (paragraphTag)

parseAndRenderHtml :: Parse Html -> (String -> String)
parseAndRenderHtml parseHtml = render . parse parseHtml
  where render (Right html) = renderHtml html
        render (Left err) = err
