import           Data.Aeson
import qualified Data.ByteString.Char8 as BS

import           Todo
import           Types

main :: IO ()
main = do
  successJson <- BS.readFile "success.json"
  failureJson <- BS.readFile "failure.json"

  putStrLn ""
  putStrLn "Success:"
  print (eitherDecodeStrict' successJson :: Either String (ListValidation List))
  -- Right (AccSuccess (List {title = Text32 {unText32 = "Grocery List"}, owner = Person {name = Text64 {unText64 = "Bob Smith"}}, items = [Item {description = Text32 {unText32 = "Apples"}, completed = True},Item {description = Text32 {unText32 = "Oranges"}, completed = False},Item {description = Text32 {unText32 = "Pears"}, completed = True}]}))

  putStrLn ""
  putStrLn "Failure:"
  print (eitherDecodeStrict' failureJson :: Either String (ListValidation List))
  -- Right (AccFailure [MustNotBeEmpty "",MustNotBeEmpty "",MustBeLessThanLength32 "Very very very very very very very very long item"])
