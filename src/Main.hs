{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Filter
import           Parser

import           ClassyPrelude         hiding (for)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Char             as C
import           Data.Monoid           (getAny)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import qualified Text.Regex.PCRE.Light as Re


mainUser, mainDomain, mainEmail :: IsString a => a
mainUser   = "erebe"
mainDomain = "erebe.eu"
mainEmail  = "erebe@erebe.eu"

virtualUser :: [Header] -> Text
virtualUser hs = T.decodeUtf8 . fromMaybe mainUser $ capitalize =<< extractUser (isFor hs)
  where
    isFor           = foldMap (\h@(Header _ str) -> if getAny (doesMatch pourDomaine h)
                                                    then [str]
                                                    else mempty)
    extractUser val = listToMaybe val >>= \s -> Re.match rPattern (T.encodeUtf8 s) [] >>= listToMaybe . drop 1
    capitalize user = if not . null $ user
                      then return $ (BC.singleton . C.toUpper $ BC.head user) <> BC.map C.toLower (BC.tail user)
                      else mempty
    rPattern        = Re.compile "([a-z._-]+)@" [Re.caseless]


deMoi :: Rule
deMoi = from $ anyOf ["romain.gerard@insa-lyon.fr", "erebe@erebe.eu", "romain.gerard@erebe.eu"]

pourMoi :: Rule
pourMoi = for $ anyOf ["romain.gerard@erebe.eu", "erebe@erebe.eu"]

pourDomaine :: Rule
pourDomaine = for $ anyOf ["@erebe.eu"]

atos :: Rule
atos = for $ anyOf ["@amesys.fr", "@atos.net", "@bull.net"]

famille :: Rule
famille = from $ anyOf ["laetitiagerard25@gmail.com", "maider.gerard313@gmail.com"]

wyplay :: Rule
wyplay = for $ anyOf ["wyplay@erebe.eu"]

insa :: Rule
insa = for $ anyOf ["@led.insa-lyon.fr", "@insa-lyon.fr", "@insalien.org", "@listes.insa-lyon.fr"]

orgaIF :: Rule
orgaIF = subject $ anyOf ["[BdE - Equipe Orga IF]"]

bde :: Rule
bde = subject $ anyOf ["[ BdE -"]

devNull :: Rule
devNull = for $ anyOf ["devnull@"]

tabulaRasa :: Rule
tabulaRasa = for $ anyOf ["tabula.rasa@erebe.eu", "editeur.algo@erebe.eu"]

haskell :: Rule
haskell = mailingList $ anyOf ["haskell"]

haskellCafe :: Rule
haskellCafe = mailingList $ anyOf ["haskell-cafe" ]

haskellBeg :: Rule
haskellBeg = mailingList $ anyOf ["beginners.haskell.org"]

blacklist :: Rule
blacklist =    from (anyOf [".Meds="])
            <> for (anyOf ["mediapart@"])

main :: IO ()
main = do
    hs <- getHeaders <$> BL.getContents

    let outputPath = find (not . T.null) $ runFilter hs <$> filters
    let path = fromMaybe defaultMailbox outputPath
    putStrLn path

    where
      defaultMailbox = ".Alpha/"
      filters = [
            -- Blacklist
             [blacklist]    ->> const "/dev/null"

            -- Perso
          ,  [deMoi]        ->> const ".Moi/"
          ,  [famille]      ->> const ".Famille/"

            -- Mailing List
          ,  [haskellCafe]  ->> const ".Mailing.Haskell-Cafe/"
          ,  [haskellBeg]   ->> const ".Mailing.Haskell-Beginner/"

            -- Professionnel
          ,  [atos]         ->> const ".Professionnel.Bull/"
          ,  [wyplay]       ->> const ".Professionnel.Wyplay/"

            -- Scolarité
          ,  [insa, orgaIF] ->> const ".Scolarite.INSA.BdE.OrgaIF/"
          ,  [insa, bde]    ->> const ".Scolarite.INSA.BdE/"
          ,  [insa]         ->> const ".Scolarite.INSA/"

            -- ToMe
          ,  [pourMoi]      ->> const defaultMailbox
          ,  [devNull]      ->> const "/dev/null"
          ,  [tabulaRasa]   ->> const ".Compte.TabulaRasa/"
          ,  [pourDomaine]  ->> \hs -> ".Compte." <> virtualUser hs <> "/"

            -- Blackhole
          ,  mempty         ->> const defaultMailbox
          ]
