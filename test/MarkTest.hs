{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Either (isLeft)
import Data.Map as Map
import Data.String
import MarkLightParser
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
  runList [testParagraphs]
  return ()

createParseTest :: (Show a, Eq a) => Parser a -> String -> a -> String -> Test
createParseTest p desc exp input =
  TestCase
    ( assertEqual
        ("Wrong parse result: " ++ desc ++ " - Input: " ++ show input)
        (Right exp)
        (parse p "TestInput" input)
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
          (Para [Word "This", Space, Word "is", Space, Word "a", Space, Word "paragraph."])
          "This is a paragraph.",
        cParagraph
          "Correct paragraph"
          (Para [Word "This", Space, Word "is", Space, Word "also", Space, Word "a", Space, Word "paragraph."])
          "This\n\tis also a paragraph.",
        cParagraph
          "Parse only the first paragraph, and no trailing whitespace"
          (Para [Word "This"])
          "This\n\nis also a paragraph.",
        cParagraph
          "No continuation. The line is empty."
          (Para [Word "This"])
          "This\n \nab",
        cParagraph
          "Leading tab"
          (Para [Word "This",Space,Word "ab"])
          "\tThis\nab"
      ]
  where
    cParagraph = createParseTest parseParagraph
