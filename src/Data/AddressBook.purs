module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

add :: Int -> Int -> Int
add x y = x + y

type Entry
  = { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address
  = { street :: String
    , city :: String
    , state :: String
    }

type AddressBook
  = List Entry

showEntry :: Entry -> String
showEntry entry =
  entry.lastName
    <> ", "
    <> entry.firstName
    <> ": "
    <> showAddress entry.address

showAddress :: Address -> String
showAddress a = a.street <> ", " <> a.city <> ", " <> a.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

matchesName :: String -> String -> Entry -> Boolean
matchesName firstName lastName entry =
  entry.firstName
    == firstName
    && entry.lastName
    == lastName

matchesAddress :: Address -> Entry -> Boolean
matchesAddress addr entry = entry.address == addr

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter (matchesName firstName lastName)

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress addr = head <<< filter (matchesAddress addr)

complement :: forall a. (a -> Boolean) -> (a -> Boolean)
complement pred = not <<< pred

notNull :: forall a. List a -> Boolean
notNull = complement null

hasName :: String -> String -> AddressBook -> Boolean
hasName firstName lastName = notNull <<< filter (matchesName firstName lastName)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy equalNames
  where
  equalNames :: Entry -> Entry -> Boolean
  equalNames e1 e2 =
    e1.firstName
      == e2.firstName
      && e1.lastName
      == e2.lastName
