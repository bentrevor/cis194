module Main where

import CS194
import Log
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "chapter 1" $ do
    it "toDigits" $ do
      toDigits (-123) `shouldBe` []
      toDigits 123 `shouldBe` [1, 2, 3]

    it "doubleEveryOther (from right)" $ do
      doubleEveryOther [] `shouldBe` []
      doubleEveryOther [1, 1, 2, 2, 3, 3] `shouldBe` [2, 1, 4, 2, 6, 3]

    it "sumDigits" $ do
      sumDigits [] `shouldBe` 0
      sumDigits [123, 456] `shouldBe` 21

    it "validate" $ do
      validate 4012888888881881 `shouldBe` True
      validate 4012888888881880 `shouldBe` False

    it "hanoi" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
      hanoi 3 "a" "b" "c" `shouldBe` [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")]

  describe "chapter 2" $ do
    it "parseMessage" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
      parseMessage "W 49 asdf qwer" `shouldBe` LogMessage Warning 49 "asdf qwer"
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"

    it "parse" $ do
      parse "E 2 562 help help\nI 29 la la la\nW 49 asdf qwer\nThis is not in the right format"
        `shouldBe` [LogMessage (Error 2) 562 "help help",
                    LogMessage Info 29 "la la la",
                    LogMessage Warning 49 "asdf qwer",
                    Unknown "This is not in the right format"]

    it "MessageTree" $ do
      let msg3 =  LogMessage Info 3 ""
      let msg10 = LogMessage Info 10 ""
      let msg12 = LogMessage Info 12 ""
      let msg13 = LogMessage Info 13 ""
      let tree = Node (Node Leaf msg3 Leaf) msg10 (Node Leaf msg13 Leaf) in
       insert' (Unknown "") tree `shouldBe` tree

    it "MessageTree" $ do
      let msg3 =  LogMessage Info 3 ""
      let msg10 = LogMessage Info 10 ""
      let msg12 = LogMessage Info 12 ""
      let msg13 = LogMessage Info 13 ""
      let tree = Node (Node Leaf msg3 Leaf) msg10 (Node Leaf msg13 Leaf) in
       insert' msg12 tree `shouldBe` Node (Node Leaf msg3 Leaf) msg10 (Node (Node Leaf msg12 Leaf) msg13 Leaf)

    it "build" $ do
      let msg3 =  LogMessage Info 3 ""
      let msg10 = LogMessage Info 10 ""
      let msg12 = LogMessage Info 12 ""
      let msg13 = LogMessage Info 13 "" in
       build [msg3, msg12, msg13, msg10] `shouldBe` Node (Node Leaf msg3 Leaf) msg10 (Node (Node Leaf msg12 Leaf) msg13 Leaf)

    it "inOrder" $ do
      let msg3 =  LogMessage Info 3 ""
      let msg10 = LogMessage Info 10 ""
      let msg12 = LogMessage Info 12 ""
      let msg13 = LogMessage Info 13 "" in
       inOrder (Node (Node Leaf msg3 Leaf) msg10 (Node (Node Leaf msg12 Leaf) msg13 Leaf)) `shouldBe` [msg3, msg10, msg12, msg13]

    it "inOrder" $ do
      let msg3 =  LogMessage Info 3 ""
      let msg10 = LogMessage Info 10 ""
      let msg12 = LogMessage Info 12 ""
      let msg13 = LogMessage Info 13 ""
      let ms = [msg3, msg10, msg12, msg13] in
       inOrder (build ms) `shouldBe` ms


    it "whatWentWrong" $ do
      let messages = [ LogMessage Info 6 "Completed armadillo processing",
                       LogMessage Info 1 "Nothing to report",
                       LogMessage (Error 99) 10 "Flange failed!",
                       LogMessage Info 4 "Everything normal",
                       LogMessage Info 11 "Initiating self-destruct sequence",
                       LogMessage (Error 70) 3 "Way too many pickles",
                       LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
                       LogMessage Warning 5 "Flange is due for a check-up",
                       LogMessage Info 7 "Out for lunch, back in two time steps",
                       LogMessage (Error 20) 2 "Too many pickles",
                       LogMessage Info 9 "Back from lunch" ] in
       whatWentWrong messages `shouldBe` [ "Way too many pickles" , "Bad pickle-flange interaction detected" , "Flange failed!" ]

  describe "chapter 3" $ do
    it "everyNth" $ do
      everyNth 2 "ABCD" `shouldBe` "BD"
      everyNth 1 "hello!" `shouldBe` "hello!"
      everyNth 2 "hello!" `shouldBe` "el!"
      everyNth 3 "hello!" `shouldBe` "l!"

    it "skips" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
      skips [1] `shouldBe` [[1]]
      skips [True,False] `shouldBe` [[True,False], [False]]
      skips ([] :: [Int]) `shouldBe` []

    it "localMaxima" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9,6]
      localMaxima [2,3,4,1,5] `shouldBe` [4]
      localMaxima [1,2,3,4,5] `shouldBe` []

    it "nTimes" $ do
      nTimes 5 'x' `shouldBe` "xxxxx"

    it "maxStars" $ do
      maxStars [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` 4

    it "starsForIndex" $ do
      starsForIndex [1,4,5,4,6,6,3,4,2,4,9] 4 `shouldBe` 4
      starsForIndex [1,4,5,4,6,6,3,4,2,4,9] 7 `shouldBe` 0

    it "columnForIndex" $ do
      columnForIndex [1,4,5,4,6,6,3,4,2,4,9] 4 `shouldBe` "****"
      columnForIndex [1,4,5,4,6,6,3,4,2,4,9] 6 `shouldBe` "  **"
      columnForIndex [1,4,5,4,6,6,3,4,2,4,9] 7 `shouldBe` "    "

    it "histogram" $ do
      histogram [3, 5] `shouldBe` "   * *    \n==========\n0123456789\n"
      histogram [1, 1, 1, 5] `shouldBe` "\
\ *        \n\
\ *        \n\
\ *   *    \n\
\==========\n\
\0123456789\n"
      histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "\
\    *     \n\
\    *     \n\
\    * *   \n\
\ ******  *\n\
\==========\n\
\0123456789\n"
