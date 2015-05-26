{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           ClassyPrelude                    hiding (replicateM)
import           Control.Monad                    (replicateM)
import           Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.ByteString.Char8            as BC
import           Parser
import           Test.QuickCheck

asciiChar :: String
asciiChar = ['!'..'~']

headerField :: Gen ByteString
headerField = BC.pack <$> (listOf . elements . filter (/= ':') $ asciiChar)

headerBody :: Gen ByteString
headerBody = BC.pack <$> (listOf . elements . filter (`onotElem` asString "\r\n") $ asciiChar)

genHeader :: Gen ByteString
genHeader = do
    f <- headerField
    nb <- choose (1, 10) :: Gen Int
    bs <- replicateM nb headerBody
    let body = foldMap (\b -> asByteString " " <> b <> "\r\n") bs
    return $ f <> ":" <> BC.tail body

validateHeaderParser :: Property
validateHeaderParser = forAll genHeader $ \h ->
  either (const False) (const True) (parseOnly parseHeader h)

main :: IO ()
main = quickCheckWith stdArgs {maxSuccess = 5000 } validateHeaderParser
