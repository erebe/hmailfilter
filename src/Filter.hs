{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}


module Filter where

import           ClassyPrelude hiding (for, isInfixOf)
import           Data.Monoid   (All (..), Any (..))
import           Data.Text     hiding (all, any, find)
import           Parser



newtype Rule m = Rule { doesMatch :: [Header] -> m }
instance Monoid m => Semigroup (Rule m) where
    (<>) = mappend

instance Monoid m => Monoid (Rule m) where
    mempty = Rule $ const mempty
    mappend (Rule f) (Rule f') = Rule (\h -> f h `mappend` f' h)

data Filter = Filter { rules   :: Rule All
                     , onMatch :: [Header] -> Text
                     }

class Mk m where
    mk :: Bool -> m

instance Mk Any where
    mk = Any

instance Mk All where
    mk = All
class ToFilter f where
    (->>) :: f -> ([Header] -> Text) -> Filter

instance ToFilter ([Rule Any]) where
    ruless ->> onRuleF = Filter
                        (Rule (\hs -> mk $ all (getAny . flip doesMatch hs) ruless))
                        onRuleF

instance ToFilter (Rule All) where
    ruless ->> onRuleF = Filter ruless onRuleF

runFilter :: [Header] -> Filter -> Text
runFilter hs (Filter rule onMatch')
  | getAll $ doesMatch rule hs = onMatch' hs
  | otherwise = mempty

match :: [HeaderName] -> (Text -> Bool) -> [Header] -> Bool
match validHeaders f =
  getAny
  . foldMap (\(Header headerName str) ->
                if headerName `elem` validHeaders
                then  Any (f str) else mempty
            )

for :: Mk m => (Text -> Bool) -> Rule m
for f = Rule $ mk . match [OriginalTo, To, Cc, Bcc] f

from :: Mk m => (Text -> Bool) -> Rule m
from f = Rule $ mk . match [From] f

subject :: (Text -> Bool) -> Rule Any
subject f = Rule $ mk . match [Subject] f

originalTo :: (Text -> Bool) -> Rule Any
originalTo f = Rule $ mk . match [OriginalTo] f

mailingList :: (Text -> Bool) -> Rule Any
mailingList f = Rule $ mk . match [ListID] f

anyOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Bool
anyOf oneOf m = any (`isInfixOf` m) oneOf

allOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Bool
allOf oneOf m = all (`isInfixOf` m) oneOf
