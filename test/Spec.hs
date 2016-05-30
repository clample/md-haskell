import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import MarkdownParser
import ParseUtil
import Html
main :: IO ()
main = defaultMain tests

tests = [
  testGroup "Parse links" [
      testCase "parseLinkTest" parseLinkTest
      ]
  ]

parseLinkTest = (assertEqual "Parse valid link" 1 1)
                          -- A {linkUrl="https://google.com", linkText="Google"},
                          -- parse parseLink "[Google](https://google.com)")
