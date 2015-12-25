{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}


module Rule where

import           ClassyPrelude hiding (for, isInfixOf)
import           Data.Monoid   (All (..), Any (..))
import           Data.Text     hiding (all, any, find)
import           Parser



newtype Match m = Match { doesMatch :: [Header] -> m }
instance Monoid m => Semigroup (Match m) where
    (<>) = mappend

instance Monoid m => Monoid (Match m) where
    mempty = Match $ const mempty
    mappend (Match f) (Match f') = Match (\h -> f h `mappend` f' h)

data Rule = Rule { rules       :: Match All
                     , onMatch :: [Header] -> Text
                     }

class Mk m where
    mk :: Bool -> m

instance Mk Any where
    mk = Any

instance Mk All where
    mk = All
class ToRule f where
    (->>) :: f -> ([Header] -> Text) -> Rule

instance ToRule ([Match Any]) where
    ruless ->> onMatchF = Rule
                        (Match (\hs -> mk $ all (getAny . flip doesMatch hs) ruless))
                        onMatchF

instance ToRule (Match All) where
    ruless ->> onMatchF = Rule ruless onMatchF

runRule :: [Header] -> Rule -> Text
runRule hs (Rule rule onMatch')
  | getAll $ doesMatch rule hs = onMatch' hs
  | otherwise = mempty

match :: [HeaderName] -> (Text -> Bool) -> [Header] -> Bool
match validHeaders f =
  getAny
  . foldMap (\(Header headerName str) ->
                if headerName `elem` validHeaders
                then  Any (f str) else mempty
            )

for :: Mk m => (Text -> Bool) -> Match m
for f = Match $ mk . match [OriginalTo, To, Cc, Bcc] f

from :: Mk m => (Text -> Bool) -> Match m
from f = Match $ mk . match [From] f

subject :: (Text -> Bool) -> Match Any
subject f = Match $ mk . match [Subject] f

originalTo :: (Text -> Bool) -> Match Any
originalTo f = Match $ mk . match [OriginalTo] f

mailingList :: (Text -> Bool) -> Match Any
mailingList f = Match $ mk . match [ListID] f

anyOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Bool
anyOf oneOf m = any (`isInfixOf` m) oneOf

allOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Bool
allOf oneOf m = all (`isInfixOf` m) oneOf
