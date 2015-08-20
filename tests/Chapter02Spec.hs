module Chapter02Spec where

import Chapter02
import Log
import Test.Hspec

spec :: Spec
spec = do
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
