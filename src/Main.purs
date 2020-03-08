module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

data Greeting = Hello | Goodbye

instance showGreeting :: Show Greeting where
  show Hello = "Hello"
  show Goodbye = "Goodbye"

toString :: forall a. (Show a) => a -> String
toString x = show x

main :: Effect Unit
main = do
  log (toString Hello)

-- | greets whatever is passed in
greet :: forall a. (Show a) => a -> String
greet = \x -> "Hello " <> show x