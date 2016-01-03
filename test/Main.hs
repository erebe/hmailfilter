{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           ClassyPrelude                    hiding (replicateM)
import           Control.Monad                    (replicateM)
import           Data.Attoparsec.ByteString.Char8 as PC
import qualified Data.ByteString.Char8            as BC
import           Parser
import           Test.QuickCheck

exHeaders :: [LByteString]
exHeaders = ["MIME-Version: 1.0\r\n"
        <> "Content-Type: multipart/mixed;\r\n"
        <> " boundary=\"=_2ef3aaa4fea5e0fe4b17a03a537a4a0c\"\r\n"
        <> "Date: Tue, 14 Apr 2015 10:20:12 +0200\n"
        <> "From: =?UTF-8?Q?Romain_G=C3=A9rard?= <romain.gerard@erebe.eu>\r\n"
        <> "To: toto@erebe.eu\r\n"
        <> "Subject: documents legals \r\n"
        <> "Reply-To: romain.gerard@erebe.eu\r\n"
        <> "Mail-Reply-To: romain.gerard@erebe.eu\n"
        <> "Message-ID: <5609177c1b14ef08893ece171818e224@erebe.eu>\r\n"
        <> "X-Sender: romain.gerard@erebe.eu\r\n"
        <> "User-Agent: Roundcube Webmail/0.9.5\n"]

asciiChar :: String
asciiChar = ['!'..'~']

headerField :: Gen ByteString
headerField = BC.pack <$> (listOf1 . elements . filter (/= ':') $ asciiChar)

headerBody :: Gen ByteString
headerBody = BC.pack <$> (listOf1 . elements . filter (`onotElem` asString "\r\n") $ asciiChar)

genHeader :: Gen ByteString
genHeader = do
    f <- headerField
    nb <- choose (1, 10) :: Gen Int
    bs <- replicateM nb headerBody
    let body = foldMap (\b -> asByteString " " <> b <> "\r\n") bs
    return $ f <> ":" <> BC.tail body

prop_HeaderParser :: Property
prop_HeaderParser = forAll genHeader $ \h ->
  either (const False) (const True) (parseOnly parseHeader h)

genHeaders :: Gen ByteString
genHeaders = do
    hs <- ofoldMap id <$> listOf genHeader
    return $ hs <> "\r\n"

prop_HeadersParser :: Property
prop_HeadersParser = forAll genHeaders $ \h ->
  either (const False) (const True) (parseOnly parseHeaders h)

prop_realHeader :: Property
prop_realHeader = forAll (elements exHeaders) $ \h ->
  check (getHeaders h)
  where
    check hs = (\(hs',uns) -> length hs' == 5 && length uns == 6 ) $
               partition (\h ->
                         case h of
                         Header (Unknown _) _ -> False
                         _ -> True)
               $ foldl' (<>) [] hs


return []
main :: IO Bool
main = do
    ret <- $(quickCheckAll)
    if ret then return ret else error "tests failed"
