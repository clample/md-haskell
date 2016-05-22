module ParseUtil where

import Data.List
import Data.Int
import Data.Char (isSpace)

data ParseState = ParseState {
                               string :: String
                             , offset :: Int64
                             } deriving (Show)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }

newtype Parse a = Parse {
                        runParse :: ParseState -> Either String (a, ParseState)
                        }

instance Functor Parse where
  fmap f parser = parser ==> \result ->
    identity (f result)

parse :: Parse a -> String -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
  Left err -> Left err
  Right (result, _) -> Right result

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left errMessage -> Left errMessage
            Right (firstResult, newState) ->
              runParse (secondParser firstResult) newState

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ ": " ++ err

parseChar :: Parse Char
parseChar =
  getState ==> \initState ->
  case (string initState) of
    [] -> bail "no more input"
    (c:str) ->
        putState newState ==> \_ ->
        identity c
        where newState = initState { string = str, offset = newOffset }
              newOffset = offset initState + 1

peekChar :: Parse (Maybe Char)
peekChar = (firstChar . string) <$> getState
  where firstChar (c:str) = Just c
        firstChar [] = Nothing

parseWhile :: (Char -> Bool) -> Parse String
parseWhile p = (fmap p <$> peekChar) ==> \mp ->
  if mp == Just True
     then parseChar ==>
          \c -> (c:) <$> parseWhile p
  else identity []

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err


skipCharIfItExists :: Parse ()
skipCharIfItExists = peekChar ==>
                      \maybeChar -> case maybeChar of Just char -> skipChar
                                                      Nothing -> identity ()
                                    where skipChar = parseChar ==>& identity ()

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhile isSpace ==>& identity ()
