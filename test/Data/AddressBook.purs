module Test.Data.AddressBook where

import Prelude
import Data.AddressBook (removeDuplicates)
import Data.List (List(..))
import Effect (Effect)
import Test.Helpers (runTest)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main =
  runTest do
    describe "Data.AddressBook" do
      describe "removeDuplicates" do
        it "returns the AddressBook unchanged if there are no duplicates" do
          let
            theList =
              Cons
                { firstName: "Daniel"
                , lastName: "King"
                , address:
                    { street: "Palmilla"
                    , city: "San Jose"
                    , state: "California"
                    }
                }
                $ Cons
                    { firstName: "Daniel"
                    , lastName: "Some other name"
                    , address:
                        { street: "Palmilla"
                        , city: "San Jose"
                        , state: "California"
                        }
                    }
                $ Nil
          removeDuplicates theList `shouldEqual` theList
        it "removes entries with duplicate names if any exist" do
          let
            theList =
              Cons
                { firstName: "Daniel"
                , lastName: "King"
                , address:
                    { street: "Palmilla"
                    , city: "San Jose"
                    , state: "California"
                    }
                }
                $ Cons
                    { firstName: "Daniel"
                    , lastName: "King"
                    , address:
                        { street: "Palmilla"
                        , city: "San Jose"
                        , state: "California"
                        }
                    }
                $ Nil

            expected =
              Cons
                { firstName: "Daniel"
                , lastName: "King"
                , address:
                    { street: "Palmilla"
                    , city: "San Jose"
                    , state: "California"
                    }
                }
                $ Nil

            actual = removeDuplicates theList
          actual `shouldEqual` expected
