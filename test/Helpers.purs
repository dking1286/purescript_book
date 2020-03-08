module Test.Helpers where

import Prelude
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Spec (SpecT)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

runTest :: SpecT Aff Unit Identity Unit -> Effect Unit
runTest = launchAff_ <<< runSpec [ consoleReporter ]
