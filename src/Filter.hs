module Filter where

import           Data.ByteString.Char8 hiding (all, any, find)
import           Data.Monoid
import           Parser



newtype Match a = Match { doesMatch :: Header -> a }
instance Monoid m => Monoid (Match m) where
    mempty = Match $ const mempty
    mappend (Match f) (Match f') = Match (\h -> f h <> f' h)

data Filter = Filter { rules   :: [Match Any]
                     , onMatch :: [Header] -> ByteString
                     }

(->>) :: [Match Any] -> ([Header] -> ByteString) -> Filter
ms ->> f = Filter ms f

runFilter :: [Header] -> Filter -> ByteString
runFilter hs (Filter rs onMatch') = if all (\m -> any (getAny . doesMatch m) hs) rs
                                      then onMatch' hs
                                      else mempty



for :: Monoid m => (ByteString -> m) -> Match m
for f = Match $ \header ->
  case header of
   Header OriginalTo str -> f str
   Header To str         -> f str
   Header Cc str         -> f str
   Header Bcc str        -> f str
   _ -> mempty

from :: Monoid m => (ByteString -> m) -> Match m
from f = Match $ \header ->
  case header of
   Header From str -> f str
   _ -> mempty

subject :: Monoid m => (ByteString -> m) -> Match m
subject f = Match $ \header ->
  case header of
   Header Subject str -> f str
   _ -> mempty

originalTo :: Monoid m => (ByteString -> m) -> Match m
originalTo f = Match $ \h ->
  case h of
   Header OriginalTo str -> f str
   _ -> mempty

anyOf :: [ByteString] -> ByteString -> Any
anyOf oneOf m = Any $ any (`isInfixOf` m) oneOf

allOf :: [ByteString] -> ByteString -> Any
allOf oneOf m = Any $ all (`isInfixOf` m) oneOf
