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
                  ( Right A {linkUrl="https://google.com", linkText="Google"} )
                  ( parse parseLink "[Google](https://google.com)" ))

parseHeaderTest = ( assertEqual "Parse valid header"
                    ( Right Header { text="header", ordinal=1 })
                    ( parse parseHeader "# header" ))

parseCorrectHeaderOrdinalTest = ( assertEqual "Parse correct header ordinal"
                                  ( Right Header { text="header", ordinal=3 })
                                  ( parse parseHeader "### header" ))

parseImageTest = ( assertEqual "Parse image"
                   ( Right Img { altText="image", imgUrl="https://my.image.com" })
                   ( parse parseImage "![image](htts://my.image.com)" ))
