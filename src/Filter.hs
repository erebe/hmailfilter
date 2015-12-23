{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}


module Filter where

import           ClassyPrelude hiding (for, isInfixOf)
import           Data.Monoid   (Any (..))
import           Data.Text     hiding (all, any, find)
import           Parser



newtype Rule = Rule { doesMatch :: Header -> Any }
instance Semigroup Rule where
    (<>) = mappend

instance Monoid Rule where
    mempty = Rule $ const mempty
    mappend (Rule f) (Rule f') = Rule (\h -> f h `mappend` f' h)

data Filter = Filter { rules  :: [Rule]
                     , onRule :: [Header] -> Text
                     }

(->>) :: [Rule] -> ([Header] -> Text) -> Filter
ruless ->> onRuleF = Filter ruless onRuleF

runFilter :: [Header] -> Filter -> Text
runFilter hs (Filter rs onRule')
  | all (\rule -> any (getAny . doesMatch rule) hs) rs = onRule' hs
  | otherwise = mempty

match :: [HeaderName] -> (Text -> Bool) -> Header -> Any
match validHeaders f (Header headerName str)
  | headerName `elem` validHeaders = Any (f str)
  | otherwise = mempty

for :: (Text -> Bool) -> Rule
for = Rule . match [OriginalTo, To, Cc, Bcc]

from :: (Text -> Bool) -> Rule
from = Rule . match [From]

subject :: (Text -> Bool) -> Rule
subject = Rule . match [Subject]

originalTo :: (Text -> Bool) -> Rule
originalTo = Rule . match [OriginalTo]

mailingList :: (Text -> Bool) -> Rule
mailingList = Rule . match [ListID]

anyOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Bool
anyOf oneOf m = any (`isInfixOf` m) oneOf

allOf :: (MonoTraversable t, Element t ~ Text) => t -> Text -> Bool
allOf oneOf m = all (`isInfixOf` m) oneOf
