{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}


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
ruless ->> onMatchF = Filter ruless onMatchF

runFilter :: [Header] -> Filter -> ByteString
runFilter hs (Filter rs onMatch')
  | all (\rule -> any (getAny . doesMatch rule) hs) rs = onMatch' hs
  | otherwise = mempty

match :: Monoid m => [HeaderName] -> (Text -> m) -> Header -> m
match validHeaders f (Header headerName str)
  | headerName `elem` validHeaders = f str
  | otherwise = mempty

for :: Monoid m => (Text -> m) -> Match m
for = Match . match [OriginalTo, To, Cc, Bcc]

from :: Monoid m => (Text -> m) -> Match m
from = Match . match [From]

subject :: Monoid m => (Text -> m) -> Match m
subject = Match . match [Subject]

originalTo :: Monoid m => (Text -> m) -> Match m
originalTo = Match . match [OriginalTo]

mailingList :: Monoid m => (Text -> m) -> Match m
mailingList = Match . match [ListID]

anyOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Any
anyOf oneOf m = Any $ any (`isInfixOf` m) oneOf

allOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Any
allOf oneOf m = Any $ all (`isInfixOf` m) oneOf
