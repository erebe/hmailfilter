{-# LANGUAGE NoImplicitPrelude #-}

module Filter where

import           ClassyPrelude hiding (for, isInfixOf)
import           Data.Monoid   (Any (..))
import           Data.Text     hiding (all, any, find)
import           Parser



newtype Match a = Match { doesMatch :: Header -> a }
instance Monoid m => Semigroup (Match m) where
    (<>) = mappend

instance Monoid m => Monoid (Match m) where
    mempty = Match $ const mempty
    mappend (Match f) (Match f') = Match (\h -> f h `mappend` f' h)

data Filter = Filter { rules   :: [Match Any]
                     , onMatch :: [Header] -> ByteString
                     }

(->>) :: [Match Any] -> ([Header] -> ByteString) -> Filter
ms ->> f = Filter ms f

runFilter :: [Header] -> Filter -> ByteString
runFilter hs (Filter rs onMatch') = if all (\m -> any (getAny . doesMatch m) hs) rs
                                    then onMatch' hs
                                    else mempty



for :: Monoid m => (Text -> m) -> Match m
for f = Match $ \header ->
  case header of
   Header OriginalTo str -> f str
   Header To str         -> f str
   Header Cc str         -> f str
   Header Bcc str        -> f str
   _ -> mempty

from :: Monoid m => (Text -> m) -> Match m
from f = Match $ \header ->
  case header of
   Header From str -> f str
   _ -> mempty

subject :: Monoid m => (Text -> m) -> Match m
subject f = Match $ \header ->
  case header of
   Header Subject str -> f str
   _ -> mempty

originalTo :: Monoid m => (Text -> m) -> Match m
originalTo f = Match $ \h ->
  case h of
   Header OriginalTo str -> f str
   _ -> mempty

mailingList :: Monoid m => (Text -> m) -> Match m
mailingList f = Match $ \h ->
  case h of
   Header ListID str -> f str
   _ -> mempty

anyOf :: [Text] -> Text -> Any
anyOf oneOf m = Any $ any (`isInfixOf` m) oneOf

allOf :: [Text] -> Text -> Any
allOf oneOf m = Any $ all (`isInfixOf` m) oneOf
