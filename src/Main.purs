module Main where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Encoding as Encoding
import Node.FS.Aff as Fs
import Node.Process as Process

main :: Effect Unit
main = Aff.launchAff_ do
  argv <- liftEffect (Process.argv)
  fileMaybe <- pure (Array.index argv 2)
  file <-
    Maybe.maybe
      (Aff.throwError (Aff.error "No file"))
      pure
      fileMaybe
  text <- Fs.readTextFile Encoding.UTF8 file
  Console.log text
