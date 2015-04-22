{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Filter
import           Parser

import           Data.ByteString.Char8 hiding (all, any, find)
import           Data.Char
import           Data.List             (find)
import           Data.Maybe
import           Data.Monoid
import           Prelude               hiding (dropWhile, elem, getContents,
                                        head, map, null, putStrLn, reverse,
                                        tail, takeWhile)



virtualUser :: [Header] -> ByteString
virtualUser = capitalize . sanitize . extractUser . isFor
  where
    isFor           = foldMap (doesMatch . for $ (\h -> if "@erebe.eu" `isInfixOf` h then [h] else mempty))
    extractUser val = fst . breakSubstring "@erebe.eu" $ fromMaybe "erebe@erebe.eu" (listToMaybe val)
    sanitize        = reverse . map (\c -> if isLetter c then toLower c else '_')
                      . takeWhile (\c -> isLetter c || elem c "._") . reverse
    capitalize str  = if not . null $ str
                     then (singleton . toUpper $ head str) <> tail str
                     else mempty


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
    hs <- getHeaders <$> getContents

    let outputPath = find (not . null) $ runFilter hs <$> filters
    let path = fromMaybe "./" outputPath
    putStrLn path

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

