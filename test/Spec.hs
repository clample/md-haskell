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
      ],
  testGroup "Parse Images" [
      testCase "parseImageTest" parseImageTest
      ],
  testGroup "Parse unordered lists" [
      testCase "parseUnorderedListTest" parseUnorderedListTest
      ],
  testGroup "Parse paragraphs" [
      testCase "parseParagraphTest" parseParagraphTest,
      testCase "parseParagraphWithBreakTest" parseParagraphWithBreakTest
      ],
  testGroup "Parse block elements with inline elements inside" [
      testCase "parseHeaderWithLinkTest" parseHeaderWithLinkTest,
      testCase "parseUnorderedListWithLinksTest" parseUnorderedListWithLinksTest,
      testCase "parseParagraphWithLinkTest" parseParagraphWithLinkTest
      ]
  ]

parseLinkTest = ( assertEqual "Parse valid link"
                  ( "<p><a href=\"https://google.com\">Google</a></p>" )
                  ( parseAndRenderHtml parseMarkdown "[Google](https://google.com)" ))

parseHeaderTest = ( assertEqual "Parse valid header"
                    ( "<h1>header</h1>" )
                    ( parseAndRenderHtml parseHeader "# header" ))

parseCorrectHeaderOrdinalTest = ( assertEqual "Parse correct header ordinal"
                                  ( "<h3>header</h3>" )
                                  ( parseAndRenderHtml parseHeader "### header" ))

parseImageTest = ( assertEqual "Parse image"
                   ( "<img src=\"https://my.image.com\" alt=\"image\">")
                   ( parseAndRenderHtml parseImage "![image](https://my.image.com)" ))

parseUnorderedListTest = ( assertEqual "Parse unordered list"
                           ( "<ul><li>First</li><li>Second</li></ul>")
                           ( parseAndRenderHtml parseUnorderedList
                             "* First\n* Second"))

parseParagraphTest = ( assertEqual "Parse paragraph"
                       ("<p>This is some great content</p>")
                       ( parseAndRenderHtml parseParagraph
                         "This is some great content"))

parseParagraphWithBreakTest = ( assertEqual "Parse paragraph with break"
                                ("<p>Some stuff<br>and more stuff</p>")
                                (parseAndRenderHtml parseParagraph
                                 "Some stuff\nand more stuff"))

parseHeaderWithLinkTest = ( assertEqual "Parse Header with link"
                            ( "<h1><a href=\"https://google.com\">Google</a></h1>")
                            ( parseAndRenderHtml parseMarkdown
                              "# [Google](https://google.com)"))


parseUnorderedListWithLinksTest = ( assertEqual "Parse unordered list with links"
                                    ( "<ul><li><a href=\"https://clample.com\">Google</a></li><li>Other content</li></ul>")
                                    ( parseAndRenderHtml parseMarkdown
                                      "* [Google](https://clample.com)\n* Other content"))

parseParagraphWithLinkTest = ( assertEqual "Parse paragraph with link"
                               ("<p>Check out the website <a href=\"https://google.com\">Google</a> and click</p>")
                               ( parseAndRenderHtml parseMarkdown
                                 "Check out the website [Google](https://google.com) and click"))

