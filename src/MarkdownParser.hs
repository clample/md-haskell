module MarkdownParser where

import Data.List
import Data.Int
import Html
import Data.Char (isSpace)
import ParseUtil
import Control.Monad.Trans.Class

parseHeader :: Parse Html
parseHeader = ((length) <$> (parseWhile (\c -> c == '#'))) >>=
              \ordinal -> skipSpaces >>
              parseInline >>=
              \inlineHtml -> skipCharIfItExists >>
              return (Node (hTag ordinal) (inlineHtml:[]))


parseLinkWithFailure :: MaybeT Parse Html
parseLinkWithFailure = lift getState >>=
                       \initState -> parseOrFail '[' initState >>
                       lift (parseWhile (\c -> c /= '\n' && c /= ']')) >>=
                       \linkText -> parseOrFail ']' initState >>
                       parseOrFail '(' initState >>
                       lift (parseWhile (\c -> c /= '\n' && c /= ')')) >>=
                       \linkUrl -> parseOrFail ')' initState >>
                       (return (Node (aTag linkUrl) ((Content {render = linkText}):[])))
                       where parseOrFail char initState = (lift peekChar) >>=
                                                \mc -> case mc of
                                                       Just c -> if c == char
                                                                 then (lift parseChar)
                                                                 else fail ""
                                                       Nothing -> lift (putState initState) >>
                                                                  fail ""
                             
parseImage :: Parse Html
parseImage = skipExclamation >>
             skipBracket >>
             parseWhile (\c -> c /= ']') >>=
             \altText -> skipBracket >>
             skipParenthesis >>
             parseWhile (\c -> c /= ')') >>=
             \imgUrl -> skipParenthesis >>
             return ((imgContent altText imgUrl))
             where skipExclamation = parseChar
                   skipParenthesis = parseChar
                   skipBracket = parseChar

parseUnorderedList :: Parse Html
parseUnorderedList = peekChar >>=
                     \maybeChar -> case maybeChar of
                                   Just '*' -> parseUnorderedListElement >>=
                                               \li -> parseUnorderedList >>=
                                               \(Node tag ul) -> return (Node ulTag (li:ul))
                                   Just _ -> return (Node ulTag [])
                                   Nothing -> return (Node ulTag [])
                                 

parseUnorderedListElement :: Parse Html
parseUnorderedListElement = skipSpecialChar >>
                            skipSpace >>
                            parseInline >>=
                            \html -> skipNewline >>
                            return (Node liTag (html:[]) )
                            where skipSpecialChar = parseChar
                                  skipSpace = parseChar
                                  skipNewline = skipCharIfItExists

parseParagraph :: Parse Html
parseParagraph = parseParagraphContent >>=
                 \htmlArr -> return (Node pTag htmlArr)
                 where parseParagraphContent = parseInline >>=
                                               \html -> skipCharIfItExists >>
                                               peekChar >>=
                                               \maybeChar -> case maybeChar of
                                                             Just '\n' -> skipCharIfItExists >>
                                                                          return (html:[])
                                                             Just _ -> parseParagraphContent >>=
                                                                       \moreHtml -> return (html:(Content {render = "<br>"}):moreHtml)
                                                             Nothing -> return (html:[])
                                                                    
parseInline :: Parse Html
parseInline = parseWhile (\c -> not (c `elem` dispatch)) >>=
              \content -> peekChar >>=
              \maybeChar -> case maybeChar of
                            Nothing -> return (Content {render = content})
                            Just '\n' -> return (Content {render = content})
                            Just '[' -> runMaybeT parseLinkWithFailure >>=
                                        \maybeLink -> case maybeLink of
                                                      Just html -> parseInline >>=
                                                                   \h -> return (HtmlArr ((Content {render = content}):html:h:[]))
                                                      Nothing -> parseChar >>=
                                                                 \c -> parseInline >>=
                                                                 \html -> return (HtmlArr ((Content {render = content ++ [c]}):html:[]))
                            Just _ -> fail "Did you match the case of the dispatched character in parseInline?"
              where dispatch = '[':'\n':[]

parseMarkdown :: Parse Html
parseMarkdown = peekChar >>=
                \maybeChar -> case maybeChar of
                              Nothing -> identity (Content {render = ""})
                              Just char -> dispatchToParser char >>=
                                           \htmlBlock -> parseMarkdown >>=
                                           \html -> case html of
                                                    (Node tag h) -> return (HtmlArr (htmlBlock:h))
                                                    content -> return (HtmlArr (htmlBlock:content:[]))
                             

dispatchToParser :: Char -> Parse Html
dispatchToParser '#' = parseHeader
dispatchToParser '*' = parseUnorderedList
dispatchToParser _ = parseParagraph 
                     

parseAndRenderHtml :: Parse Html -> (String -> String)
parseAndRenderHtml parseHtml = render . parse parseHtml
  where render (Right html) = renderHtml html
        render (Left err) = err

