import Lib
import Test.HUnit

test1 :: Test
test1 = TestCase $
    let (_, val) = runParser wordsP "abcd  ss  ps\\ asdf_"
    in  assertEqual "for wordsP" (Right ["abcd", "ss", "ps asdf_"]) val

test2 :: Test
test2 = TestCase $
    let (_, val) = runParser quotedP "'asdc\\'aaa'"
    in  assertEqual "for quotedP" (Right "asdc'aaa") val

test3 :: Test
test3 = undefined

main :: IO Counts
main = runTestTT . TestList $
    [TestLabel "wordsP" test1, TestLabel "quotedP" test2]
