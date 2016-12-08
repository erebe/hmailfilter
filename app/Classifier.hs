{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Parser
import           Rule

import           ClassyPrelude         hiding (for, singleton)
import           Control.Monad         (msum)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Char             as C
import           Data.HashMap.Strict   (singleton)
import           Data.Monoid           (All, Any, getAny)
import qualified Data.Text.Encoding    as T
import           Data.Time.LocalTime
import qualified Text.Regex.PCRE.Light as Re



mainUser, mainDomain, mainEmail :: IsString a => a
mainUser   = "erebe"
mainDomain = "erebe.eu"
mainEmail  = "erebe@erebe.eu"

virtualUser :: [Header] -> Text
virtualUser hs = T.decodeUtf8 . fromMaybe mainUser $ capitalize =<< extractUser (isFor hs)
  where
    isFor           = foldMap (\h@(Header hName str) -> if getAny (doesMatch pourDomaine (singleton hName [h]))
                                                    then [str]
                                                    else mempty)
    extractUser val = listToMaybe val >>= \s -> Re.match rPattern (T.encodeUtf8 s) [] >>= listToMaybe . drop 1
    capitalize user = if not . null $ user
                      then return $ (BC.singleton . C.toUpper $ BC.head user) <> BC.map C.toLower (BC.tail user)
                      else mempty
    rPattern        = Re.compile ("([a-z._-]+)@" <> mainDomain) [Re.caseless]


deMoi :: Match Any
deMoi = from $ anyOf ["romain.gerard@insa-lyon.fr", "erebe@erebe.eu", "romain.gerard@erebe.eu"]

pourMoi :: Match Any
pourMoi = for $ anyOf ["romain.gerard@erebe.eu", "erebe@erebe.eu"]

pourDomaine :: Match Any
pourDomaine = for $ anyOf ["@erebe.eu"]

atos :: Match Any
atos = for $ anyOf ["@amesys.fr", "@atos.net", "@bull.net", "bull@erebe.eu"]

famille :: Match Any
famille = from $ anyOf ["laetitiagerard25@gmail.com", "maider.gerard313@gmail.com"]

netdata :: Match All
netdata =    from (anyOf ["netdata@erebe.eu"])
          <> date isBackupTime
  where
    isBackupTime :: Maybe TimeOfDay -> Bool
    isBackupTime (Just time) = time > TimeOfDay 5 0 0 && time < TimeOfDay 6 0 0
    isBackupTime _           = False

insa :: Match Any
insa = for $ anyOf ["@led.insa-lyon.fr", "@insa-lyon.fr", "@insalien.org", "@listes.insa-lyon.fr"]

devNull :: Match Any
devNull = for $ anyOf ["devnull@"]

tabulaRasa :: Match Any
tabulaRasa = for $ anyOf ["tabula.rasa@erebe.eu", "editeur.algo@erebe.eu"]

haskellCafe :: Match Any
haskellCafe = mailingList $ anyOf ["haskell-cafe" ]

haskellBeg :: Match Any
haskellBeg = mailingList $ anyOf ["beginners.haskell.org"]

blacklist :: Match Any
blacklist =    from (anyOf [".Meds=", "datesmail.com"])
            <> for  (anyOf ["mediapart@"])
            <> subject (anyOf ["naked photo","new photos"," dating ", "pussy", "hot photo", "Happy New Year!"])


criteo :: Match Any
criteo = for $ anyOf ["@criteo.com"]

bes :: Match Any
bes = subject (anyOf ["BES-"])

review :: Match Any
review = from $ anyOf ["review@"]

onCall :: Match Any
onCall = from $ anyOf ["@opsgenie.net"]

jira :: Match Any
jira = from $ anyOf ["agile@criteo"]

jenkins :: Match Any
jenkins = from $ anyOf ["qabot"]

prod :: Match Any
prod = from $ anyOf ["@criteo.prod"]

preprod :: Match Any
preprod = from $ anyOf ["@criteo.preprod"]

team :: Match Any
team = from $ anyOf ["storage-monitoring@criteo"]

main :: IO ()
main = do
    headers <- getHeaders <$> BL.getContents


    let outputPath = msum $ runRule headers <$> myRules
    let path = fromMaybe defaultMailbox outputPath


    putStrLn path


    where
      defaultMailbox = ".Alpha/"
      myRules = [

           -- Blacklist
            [blacklist]    ->> const "/dev/null"

           -- Spam
          , [isSpam]       ->> const ".Spam/"
          , [netdata]      ->> const ".Spam/"

           -- Perso
          , [deMoi]        ->> const ".Moi/"
          , [famille]      ->> const ".Famille/"

           -- Mailing List
          , [haskellCafe]  ->> const ".Mailing.Haskell-Cafe/"
          , [haskellBeg]   ->> const ".Mailing.Haskell-Beginner/"

           -- Professionnel
          , [atos]         ->> const ".Professionnel.Bull/"
          , [criteo, bes]          ->> const ".Professionnel.Criteo.Bes/"
          , [criteo, review]       ->> const ".Professionnel.Criteo.Review/"
          , [criteo, onCall]       ->> const ".Professionnel.Criteo.OnCall/"
          , [criteo, jira]         ->> const ".Professionnel.Criteo.Jira/"
          , [criteo, jenkins]      ->> const ".Professionnel.Criteo.Jenkins/"
          , [criteo, team]         ->> const ".Professionnel.Criteo.Team/"
          , [criteo, prod]         ->> const ".Professionnel.Criteo.Prod/"
          , [criteo, preprod]      ->> const ".Professionnel.Criteo.Preprod/"
          , [criteo]       ->> const ".Professionnel.Criteo/"

           -- Scolarité
          , [insa]         ->> const ".Scolarite.INSA/"

           -- ToMe
          , [pourMoi]      ->> const defaultMailbox
          , [devNull]      ->> const "/dev/null"
          , [tabulaRasa]   ->> const ".Compte.TabulaRasa/"
          , [pourDomaine]  ->> \hs -> ".Compte." <> virtualUser hs <> "/"

           -- Blackhole, not necessary as there is a Maybe
          , (mempty :: Match All)  ->> const defaultMailbox
          ]
