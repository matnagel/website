{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either (isLeft)
import Data.Map as Map
import Data.String
import MarkLightParser
import MarkLight.Arguments
import Test.HUnit
import Text.Parsec
import Text.Parsec.String

runList :: [Test] -> IO ()
runList [] = return ()
runList (t : ts) = do
  stats <- runTestTT t
  if errors stats + failures stats > 0
    then return ()
    else runList ts

main :: IO ()
main = do
  runList [testParagraphs, testCommands, testArguments]
  return ()

createParseTest :: (Show a) => Parser a -> String -> String -> String -> Test
createParseTest p desc exp input =
  TestCase
    ( assertEqual
        ("Wrong parse result: " ++ desc ++ " - Input: " ++ show input)
        exp
        (case (parse p ("TestInput: " ++ input) input) of
            Right str -> show str
            Left err -> error $ show $ err
        )
    )

createParseFailTest :: Parser a -> String -> String -> Test
createParseFailTest p desc input =
  TestCase
    ( assertBool
        ("Parse should fail: " ++ desc ++ " - Input: " ++ show input)
        (isLeft $ parse p "TestInput" input)
    )

testParagraphs =
  TestLabel "Test Parse Paragraphs" $
    TestList
      [ cParagraph
          "Correct paragraph"
          "Para [Word \"This\",Space,Word \"is\",Space,Word \"a\",Space,Word \"paragraph.\"]"
          "This is a paragraph.",
        cParagraph
          "Correct paragraph"
          "Para [Word \"This\",Space,Word \"is\",Space,Word \"also\",Space,Word \"a\",Space,Word \"paragraph.\"]"
          "This\n\tis also a paragraph.",
        cParagraph
          "Parse only the first paragraph, and no trailing whitespace"
          "Para [Word \"This\"]"
          "This\n\nis also a paragraph.",
        cParagraph
          "No continuation. The line is empty."
          "Para [Word \"This\"]"
          "This\n \nab",
        cParagraph
          "Leading tab"
          "Para [Word \"This\",Space,Word \"ab\"]"
          "\tThis\nab",
        cParagraph
          "Space newline"
          "Para [Word \"This\",Space,Word \"ab\"]"
          "This \nab"
      ]
  where
    cParagraph = createParseTest parseParagraph

testCommands =
  TestLabel "Tests inline commands" $
    TestList
      [ cLink
          "Correct link"
          "Link (MkURLPath \"test.path/succ\") (MkText \"my link.\")"
          "{link path=\"test.path/succ\" text=\"my link.\"}",
        cLink
          "Correct link with newline"
          "Link (MkURLPath \"test.path/succ\") (MkText \"my link.\")"
          "{link\n path=\"test.path/succ\"\n text=\"my link.\"}",
        cHeader
          "Correct header with space"
          "Header (MkText \"My Interests\")"
          "= My Interests"

      ]
  where
    cLink = createParseTest parseLink
    cHeader = createParseTest parseHeader

data Person = MkPerson { firstname :: String, lastname :: String } deriving Show

personArg :: Argument Person
personArg = MkPerson
    <$> FromKey "fname" parseQuotedString
    <*> FromKey "lname" parseQuotedString

personArgDefault :: Argument Person
personArgDefault = MkPerson
    <$> FromKeyDefault "fname" parseQuotedString "Bill"
    <*> FromKey "lname" parseQuotedString


data Configuration = MkConfig { containsMenu :: Bool, isHome :: Bool } deriving Show

configurationArg :: Argument Configuration
configurationArg = MkConfig
    <$> FromFlag "containsMenu" id
    <*> FromKey "isHome" parseBool

testArguments = TestLabel "Tests argument" $
    TestList
      [
        cArgParser
            "Simple arguments"
            "MkPerson {firstname = \"Tom\", lastname = \"Rattle\"}"
            "fname=\"Tom\", lname=\"Rattle\"",
        cArgParser
            "Switch arguments"
            "MkPerson {firstname = \"Tom\", lastname = \"Rattle\"}"
            "lname=\"Rattle\" fname=\"Tom\" ",
        cArgDefParser
            "Default firstname"
            "MkPerson {firstname = \"Bill\", lastname = \"Rattle\"}"
            "lname=\"Rattle\" ",
        cFailArgParser
            "Double firstname"
            "fname=\"Tom\", lname=\"Rattle\", fname=\"Zippo\"",
        cFailArgParser
            "Unknown key"
            "fname=\"Tom\", lname=\"Rattle\", id=\"Zippo\"",
        cConfigParser
            "Parse Flags and Bools"
            "MkConfig {containsMenu = True, isHome = True}"
            "isHome=true, containsMenu"
      ]
  where
    cArgParser = createParseTest $ parseArg personArg
    cConfigParser = createParseTest $ parseArg configurationArg
    cFailArgParser = createParseFailTest $ parseArg personArg
    cArgDefParser = createParseTest $ parseArg personArgDefault

