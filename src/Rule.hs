{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}


module Rule where

import           ClassyPrelude       hiding (for, toList)
import           Data.HashMap.Strict
import           Data.Monoid         (All (..), Any (..))
import           Parser              (Header (..), HeaderName (..))
import Data.Time.Format


newtype Match m = Match { doesMatch :: HashMap HeaderName [Header] -> m }
instance Monoid m => Semigroup (Match m) where
    (<>) = mappend

instance Monoid m => Monoid (Match m) where
    mempty = Match $ const mempty
    mappend (Match f) (Match f') = Match (\h -> f h `mappend` f' h)

data Rule = Rule { rules   :: Match All
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

instance ToRule ([Match All]) where
    ruless ->> onMatchF = Rule (fold ruless) onMatchF

runRule :: HashMap HeaderName [Header] -> Rule -> Maybe Text
runRule hs (Rule rule onMatch')
  | getAll $ doesMatch rule hs = Just $ onMatch' $ mconcat $ snd <$> toList hs
  | otherwise = mempty

match :: [HeaderName] -> (Text -> Bool) -> HashMap HeaderName [Header] -> Bool
match validHeaders f headers =
  getAny $ ofoldMap (\headerName ->
                      let headers' = lookupDefault mempty headerName headers in
                      ofoldMap (Any . f . content) headers'
                    ) validHeaders
{-# INLINE match #-}

for :: Mk m => (Text -> Bool) -> Match m
for f = Match $ mk . match [To, Cc, Bcc] f

from :: Mk m => (Text -> Bool) -> Match m
from f = Match $ mk . match [From] f

subject :: Mk m => (Text -> Bool) -> Match m
subject f = Match $ mk . match [Subject] f

originalTo :: Mk m => (Text -> Bool) -> Match m
originalTo f = Match $ mk . match [OriginalTo] f

mailingList :: Mk m => (Text -> Bool) -> Match m
mailingList f = Match $ mk . match [ListID] f

date :: (Monad m' , Mk m, ParseTime t) => (m' t -> Bool) -> Match m
date f = Match $ mk . match [Date] (f . parseTimeM True defaultTimeLocale "%a, %e %b %Y %T %z (%Z)" . unpack)

isSpam :: Match Any
isSpam = Match $ mk . match [Spam] (`isInfixOf` "YES")

anyOf :: (MonoFoldable mono, IsSequence (Element mono), Eq (Element (Element mono))) => mono -> Element mono -> Bool
anyOf oneOf m = any (`isInfixOf` m) oneOf
{-# INLINE anyOf #-}

allOf :: (MonoFoldable mono, IsSequence (Element mono), Eq (Element (Element mono))) => mono -> Element mono -> Bool
allOf oneOf m = all (`isInfixOf` m) oneOf
{-# INLINE allOf #-}
