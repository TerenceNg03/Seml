import Lib
import Test.HUnit

test1 :: Test
test1 =
    TestCase $
        let (_, val) = runParser semlP "(a\\ c)"
         in assertEqual "Seml1" (Right [Tag "a c"]) val

test2 :: Test
test2 =
    TestCase $
        let (_, val) = runParser semlP "(a ('xx\\''))"
         in assertEqual "Seml2" (Right [Elem "a" [Rec (Tag "xx'")]]) val

test3 :: Test
test3 =
    TestCase $
        let (_, val) = runParser semlP "(a b c)"
         in assertEqual "Seml3" (Right [Elem "a" [Text "b", Text "c"]]) val

main :: IO Counts
main =
    runTestTT . TestList $
        [test1, test2, test3]
