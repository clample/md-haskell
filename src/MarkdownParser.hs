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
              identity (Node (hTag ordinal) (inlineHtml:[]))
{--
parseLink :: Parse Html
parseLink = skipBracket >>
            parseWhile (\c -> c /= ']') >>=
            \linkText -> skipBracket >>
            skipParenthesis >>
            parseWhile (\c -> c /= ')') >>=
            \linkUrl -> skipParenthesis >>
            identity (Node (aTag linkUrl) ((Content { render = linkText }):[]))
            where skipBracket = parseChar
                  skipParenthesis = parseChar
--}


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
             identity ((imgContent altText imgUrl))
             where skipExclamation = parseChar
                   skipParenthesis = parseChar
                   skipBracket = parseChar

parseUnorderedList :: Parse Html
parseUnorderedList = peekChar >>=
                     \maybeChar -> case maybeChar of
                                   Just '*' -> parseUnorderedListElement >>=
                                               \li -> parseUnorderedList >>=
                                               \(Node tag ul) -> identity (Node ulTag (li:ul))
                                   Just _ -> identity (Node ulTag [])
                                   Nothing -> identity (Node ulTag [])
                                 

parseUnorderedListElement :: Parse Html
parseUnorderedListElement = skipSpecialChar >>
                            skipSpace >>
                            parseWhile (\c -> c /= '\n') >>=
                            \content -> skipNewline >>
                            identity (Node liTag ((Content {render = content}):[]) )
                            where skipSpecialChar = parseChar
                                  skipSpace = parseChar
                                  skipNewline = skipCharIfItExists

parseParagraph :: Parse Html
parseParagraph = parseWhile (\c -> c /= '\n') >>=
                 \content -> skipCharIfItExists >>
                 peekChar >>=
                 \maybeChar -> case maybeChar of
                               Just '\n' -> skipCharIfItExists >>
                                            identity (Node pTag ((Content {render = content }):[]))
                               Just _ -> parseParagraph >>=
                                         \(Node tag ((contentTag):contentTags)) -> identity (Node pTag ((Content { render = (content ++ "<br>" ++ (render contentTag)) }):[]))
                               Nothing -> identity (Node pTag ((Content { render = content }):[]))

parseInline :: Parse Html
parseInline = parseWhile (\c -> not (c `elem` dispatch)) >>=
              \content -> peekChar >>=
              \maybeChar -> case maybeChar of
                            Nothing -> return (Content {render = content})
                            Just '\n' -> return (Content {render = content})
                            Just '[' -> runMaybeT parseLinkWithFailure >>=
                                        \maybeLink -> case maybeLink of
                                                      Just html -> parseInline >>=
                                                                   \h -> return (Node (Tag {renderOpen = "", renderClose = ""}) ((Content {render = content}):html:h:[]))
                                                      Nothing -> parseChar >>=
                                                                 \c -> parseInline >>=
                                                                 \html -> return (Node (Tag {renderOpen = "", renderClose = "" }) ((Content {render = content ++ [c]}):html:[]))
                            Just _ -> fail "Did you match the case of the dispatched character in parseInline?"
              where dispatch = '[':'\n':[]

parseMarkdown :: Parse Html
parseMarkdown = peekChar >>=
                \maybeChar -> case maybeChar of
                              Nothing -> identity (Content {render = ""})
                              Just char -> dispatchToParser char >>=
                                           \htmlBlock -> parseMarkdown >>=
                                           \html -> case html of
                                                    (Node tag h) -> return (Node (Tag {renderOpen="", renderClose=""}) (htmlBlock:h))
                                                    content -> return (Node (Tag {renderOpen="", renderClose=""}) (htmlBlock:content:[]))
                             

dispatchToParser :: Char -> Parse Html
dispatchToParser '#' = parseHeader >>=
                       \headerTag -> identity (headerTag)
dispatchToParser _ = parseParagraph >>=
                     \paragraphTag -> identity (paragraphTag)

parseAndRenderHtml :: Parse Html -> (String -> String)
parseAndRenderHtml parseHtml = render . parse parseHtml
  where render (Right html) = renderHtml html
        render (Left err) = err

