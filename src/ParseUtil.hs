module ParseUtil where

import Data.List
import Data.Int
import Data.Char (isSpace)
import Control.Monad
import Control.Monad.Trans

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
  fmap = liftM

instance Applicative Parse where
  pure = identity
  (<*>) = ap
    
instance Monad Parse where
  return = pure
  (>>) = (*>)
  (>>=) = (==>)
  fail = bail

newtype MaybeT m a = MaybeT {
                            runMaybeT :: m (Maybe a)
                            }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
  unwrapped <- runMaybeT x
  case unwrapped of
    Nothing -> return Nothing
    Just y -> runMaybeT (f y)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing


instance (Monad m) => Functor (MaybeT m) where
  fmap = liftM

instance (Monad m) => Applicative (MaybeT m) where
  pure = returnMT
  (<*>) = ap
  
instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (>>=) = bindMT
  fail = failMT

instance MonadTrans MaybeT where
  lift m = MaybeT (Just `liftM` m)

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
  getState >>= \initState ->
  case (string initState) of
    [] -> bail "no more input"
    (c:str) ->
        putState newState >>= \_ ->
        identity c
        where newState = initState { string = str, offset = newOffset }
              newOffset = offset initState + 1

peekChar :: Parse (Maybe Char)
peekChar = (firstChar . string) `liftM` getState
  where firstChar (c:str) = Just c
        firstChar [] = Nothing

parseWhile :: (Char -> Bool) -> Parse String
parseWhile p = (fmap p `liftM` peekChar) >>= \mp ->
  if mp == Just True
     then parseChar >>=
          \c -> (c:) <$> parseWhile p
  else identity []

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err


skipCharIfItExists :: Parse ()
skipCharIfItExists = peekChar >>=
                      \maybeChar -> case maybeChar of Just char -> skipChar
                                                      Nothing -> identity ()
                                    where skipChar = parseChar >> identity ()

skipSpaces :: Parse ()
skipSpaces = parseWhile isSpace >> identity ()

