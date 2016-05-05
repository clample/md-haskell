import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Lib

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
            testProperty "prop_identity" prop_identity
            ]
        ]

prop_identity str = parseMarkdown str == str
