module Main where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.FS.Aff as Fs
import Node.Process as Process
import Node.Stream as Stream

readFromStream :: Stream.Readable () -> Aff.Aff String
readFromStream r =
  Aff.makeAff
    (\callback -> do
      ref <- Ref.new ""
      Stream.onDataString r Encoding.UTF8 \s -> do
        buffer <- Ref.read ref
        Ref.write (buffer <> s) ref
      Stream.onEnd r do
        buffer <- Ref.read ref
        callback (pure buffer)
      pure mempty)

main :: Effect Unit
main = Aff.launchAff_ do
  argv <- liftEffect (Process.argv)
  fileMaybe <- pure (Array.index argv 2)
  text <-
    Maybe.maybe
      (readFromStream Process.stdin)
      (\file -> Fs.readTextFile Encoding.UTF8 file)
      fileMaybe
  Console.log text
