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

parseUnsafe :: Show er => (String -> Either er a) -> String -> a
parseUnsafe p str =
  case p str of
    Left er -> error $ show er
    Right exp -> exp

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
        (case (parse p "TestInput" input) of
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
          "{link\n path=\"test.path/succ\"\n text=\"my link.\"}"
      ]
  where
    cLink = createParseTest parseLink

data Person = MkPerson { firstname :: String, lastname :: String } deriving Show

personArg :: Argument Person
personArg = FromKey "lname" parseQuotedString $
    FromKey "fname" parseQuotedString $
    (Lift MkPerson)

testArguments = TestLabel "Tests argument" $
    TestList
      [
--         cArguments
--           "Basic Arguments"
--           "(name=\"me\", url=\"www.bla\")"
--           "url=\"www.bla\" name=\"me\"",
--         cArguments
--           "Arguments comma separated"
--           "(id=\"tag\", name=\"me\", opt=\"dog\", url=\"www.bla\")"
--           "url=\"www.bla\",name=\"me\", id=\"tag\" , opt=\"dog\"",
--         cDefaultArguments
--           "Default arguments"
--           "(id=\"tag\", name=\"me\", url=\"www.bla\")"
--           "\"www.bla\" \"me\", id=\"tag\"",
--         cDefaultArguments
--           "Necessary second argument is not named."
--           "(id=\"tag\", name=\"me\", url=\"www.bla\")"
--           "url=\"www.bla\" \"me\", id=\"tag\"",
--         cArguments
--           "Setting key present"
--           "(id=True)"
--           "id",
--         cMissingArguments
--           "Not all arguments present"
--           "\"me\", id=\"tag\"",
--         cUnknownArguments
--           "Not all arguments present"
--           "id=\"tag\"",
        cArgParser
            "Simple arguments"
            "MkPerson {firstname = \"Tom\", lastname = \"Rattle\"}"
            "fname=\"Tom\", lname=\"Rattle\""
      ]
  where
    cArgParser = createParseTest $ parseArg personArg

