import Test.HUnit

import Echo.Agent

testHamming = TestCase (assertEqual "hamming [1,2,3] [2,2,4]" 2 (hamming [1, 2, 3] [2, 2, 4]))
testHamming1Gt2 = TestCase (assertEqual "hamming [1,2,3,4] [2,2,4]" 3 (hamming [1, 2, 3, 4] [2, 2, 4]))
testHamming2Gt1 = TestCase (assertEqual "hamming [1,2,3] [2,2,4,4]" 3 (hamming [1, 2, 3] [2, 2, 4, 4]))


tests = TestList [TestLabel "testhamming" testHamming
                , TestLabel "testhamming1gt2" testHamming1Gt2
                , TestLabel "testhamming2gt1" testHamming2Gt1]

main = do
    runTestTT tests
