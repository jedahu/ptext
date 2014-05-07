module PText where

import Debug.Trace

import Prelude
import Data.Either
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.Reader.Trans
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String

foreign import stringCompare
  "function stringCompare(s1) {\
  \  return function(s2) {\
  \    var x = s1.localeCompare(s2);\
  \    if (x === 0) return Prelude.EQ;\
  \    if (x > 0) return Prelude.GT;\
  \    return Prelude.LT;\
  \  };\
  \}" :: String -> String -> Ordering

data Tree = Literal String
          | Bounded String Tree
          | Inline [Tree]

instance showTree :: Show Tree where
    show (Literal s) = "Literal " ++ show s
    show (Bounded s t) = "Bounded " ++ show s ++ " " ++ show t

data Bound = ParsedBound String | LiteralBound String

data Bounds = Bounds (M.Map String Bound)

type PParser r m = ParserT String (ReaderT r m) Tree

data Scope = Scope
           { bounds :: Bounds
           , escape :: String
           }

defaultBounds :: Bounds
defaultBounds = Bounds $ M.fromList
              [ Tuple "/" (ParsedBound "italic")
              , Tuple "*" (ParsedBound "bold")
              , Tuple "=" (ParsedBound "fixed")
              , Tuple "`" (LiteralBound "code")
              , Tuple "$" (LiteralBound "math")
              ]

defaultScope :: Scope
defaultScope = Scope
             { bounds: defaultBounds
             , escape: "\\"}

instance monadReaderParserT ::
  (Monad m, MonadReader r m) => MonadReader r (ParserT s m) where
    ask = lift ask
    local f = mapParserT (local f)
        where
            mapParserT f (ParserT m) = ParserT $ mapStateT f m

instance ordString :: Ord String where
    compare = stringCompare


(<*) :: forall f a b. (Applicative f) => f a -> f b -> f a
(<*) u v = pure const <*> u <*> v

(*>) :: forall f a b. (Applicative f) => f a -> f b -> f b
(*>) u v = pure (const id) <*> u <*> v

manyTill :: forall s m end a. (Monad m) =>
            ParserT s m a -> ParserT s m end -> ParserT s m [a]
manyTill p end = scan {}
  where
    scan _ = base {} <|> recur {}
    base _ = do
        end
        return []
    recur _ = do
        x <- p
        xs <- scan {}
        return (x:xs)

many1Till :: forall s m end a. (Monad m) =>
             ParserT s m a -> ParserT s m end -> ParserT s m [a]
many1Till p end = (:) <$> p <*> manyTill p end

escaped :: forall m. (Monad m) => PParser Scope m
escaped = do
    Scope scope <- ask
    Literal <$> (string scope.escape *> char)

bounded :: forall m. (Monad m) =>
           PParser Scope m -> PParser Scope m
bounded p = do
    c <- char
    Scope { bounds = Bounds bs } <- ask
    maybe
      (fail "No bound")
      (parse c p)
      (M.lookup c bs)
  where
    parse c p (ParsedBound name) =
        (Bounded name <<< Inline)
        <$> many1Till p (try (string c))
    parse c _ (LiteralBound name) =
        (Bounded name <<< Literal <<< foldMap id)
        <$> many1Till char (try (string c))

inline :: forall m. (Monad m) => PParser Scope m
inline = inline_r {}

inline_r :: forall m. (Monad m) => {} -> PParser Scope m
inline_r _ = bounded (inline_r {}) <|> text <?> "Not inline"

ptest :: forall a. (Show a) => ParserT String Identity a -> String -> String
ptest p s = case (runIdentity $ runParserT s p) of
    Left (ParseError e) -> e.message
    Right x -> show x

parseText :: String -> String
parseText s = case result of
    Left (ParseError e) -> e.message
    Right x -> show x
  where
    result = runIdentity $ runReaderT (runParserT s $ bounded inline) defaultScope
