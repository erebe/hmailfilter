{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Filter
import           Parser

import           ClassyPrelude         hiding (for)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char             as C
import           Data.Monoid           (Any)
import qualified Text.Regex.PCRE.Light as Re


mainUser :: ByteString
mainUser = BC.pack "erebe"

mainDomain :: ByteString
mainDomain = BC.pack "erebe.eu"

mainEmail :: ByteString
mainEmail = BC.pack "erebe@erebe.eu"

virtualUser :: [Header] -> ByteString
virtualUser hs =  fromMaybe mainUser $ capitalize =<< extractUser (isFor hs)
  where
    isFor           = foldMap (doesMatch . for $ (\h -> if ("@" <> mainDomain) `isInfixOf` h then [h] else mempty))
    extractUser val = listToMaybe val >>= \s -> Re.match rPattern s [] >>= listToMaybe . drop 1
    capitalize user = if not . null $ user
                      then return $ (BC.singleton . C.toUpper $ BC.head user) <> BC.map C.toLower (BC.tail user)
                      else mempty
    rPattern        = Re.compile "([a-z._-]+)@" [Re.caseless]


deMoi :: Match Any
deMoi = from $ anyOf ["romain.gerard@insa-lyon.fr", "erebe@erebe.eu", "romain.gerard@erebe.eu"]

pourMoi :: Match Any
pourMoi = for $ anyOf ["romain.gerard@erebe.eu", "erebe@erebe.eu"]

pourDomaine :: Match Any
pourDomaine = for $ anyOf ["@erebe.eu"]

atos :: Match Any
atos = for $ anyOf ["@amesys.fr", "@atos.net", "@bull.net"]

famille :: Match Any
famille = from $ anyOf ["laetitiagerard25@gmail.com", "maider.gerard313@gmail.com"]

haskell :: Match Any
haskell = for $ anyOf ["haskell@erebe.eu"]

wyplay :: Match Any
wyplay = for $ anyOf ["wyplay@erebe.eu"]

insa :: Match Any
insa = for $ anyOf ["@insa-lyon.fr", "@insalien.org", "@listes.insa-lyon.fr"]

orgaIF :: Match Any
orgaIF = subject $ anyOf ["[BdE - Equipe Orga IF]"]

bde :: Match Any
bde = subject $ anyOf ["[ BdE -"]

devNull :: Match Any
devNull = for $ anyOf ["devnull@"]

tabulaRasa :: Match Any
tabulaRasa = for $ anyOf ["tabula.rasa@erebe.eu", "editeur.algo@erebe.eu"]

main :: IO ()
main = do
    hs <- getHeaders <$> BC.getContents

    let outputPath = find (not . BC.null) $ runFilter hs <$> filters
    let path = fromMaybe "./" outputPath
    BC.putStrLn path

    where
      filters = [
            -- Perso
             [deMoi]        ->> const ".Moi/"
          ,  [famille]      ->> const ".Famille/"

            --Professionnel
          ,  [atos]         ->> const ".Professionnel.Bull/"
          ,  [wyplay]       ->> const ".Professionnel.Wyplay/"

            -- Scolarité
          ,  [insa, orgaIF] ->> const ".Scolarite.INSA.BdE.OrgaIF/"
          ,  [insa, bde]    ->> const ".Scolarite.INSA.BdE/"
          ,  [insa]         ->> const ".Scolarite.INSA/"

            -- ToMe
          ,  [pourMoi]      ->> const "./"
          ,  [devNull]      ->> const "/dev/null"
          ,  [tabulaRasa]   ->> const ".Compte.TabulaRasa/"
          ,  [pourDomaine]  ->> \hs -> ".Compte." <> virtualUser hs <> "/"

            -- Mailing
          -- ,  [haskell]      ->> const ".Compte.Haskell/"

            -- Blackhole
          ,  mempty         ->> const "./"
          ]

