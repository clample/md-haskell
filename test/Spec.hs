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
      ],
  testGroup "Parse headers" [
      testCase "parseHeaderTest" parseHeaderTest,
      testCase "parseCorrectHeaderOrdinal" parseCorrectHeaderOrdinalTest
      ]
  ]

parseLinkTest = ( assertEqual "Parse valid link"
                  ( "<a href=\"https://google.com\">Google</a>" )
                  ( parseAndRenderHtml parseLink "[Google](https://google.com)" ))

parseHeaderTest = ( assertEqual "Parse valid header"
                    ( "<h1>header</h1>" )
                    ( parseAndRenderHtml parseHeader "# header" ))

parseCorrectHeaderOrdinalTest = ( assertEqual "Parse correct header ordinal"
                                  ( "<h3>header</h3>" )
                                  ( parseAndRenderHtml parseHeader "### header" ))

parseImageTest = ( assertEqual "Parse image"
                   ( "<img src=\"https://my.image.com\" alt=\"image\">")
                   ( parseAndRenderHtml parseImage "![image](htts://my.image.com)" ))
