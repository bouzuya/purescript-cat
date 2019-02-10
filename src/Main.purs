module Main where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Data.String as String
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Node.Encoding as Encoding
import Node.FS.Aff as Fs
import Node.Process as Process
import Node.Stream as Stream
import Record as Record

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

readOptions :: Array String -> { files :: Array String, number :: Boolean }
readOptions argv = do
  Record.merge
    { files:
        Array.filter
          (Maybe.isNothing <<< (String.indexOf (String.Pattern "--")))
          argv
    }
    (Array.foldl
      (\o s ->
        case s of
          "--number" -> Record.merge { number: true } o
          _ -> o
      )
      { number: false }
      argv)

main :: Effect Unit
main = Aff.launchAff_ do
  argv <- liftEffect (Process.argv)
  options <- pure (readOptions (Array.drop 2 argv))
  text <-
    Maybe.maybe
      (readFromStream Process.stdin)
      (\file -> Fs.readTextFile Encoding.UTF8 file)
      (Array.index options.files 0)
  let
    pad5 n
      | n < 10 = "    " <> show n
      | n < 100 = "   " <> show n
      | n < 1000 = "  " <> show n
      | n < 10000 = " " <> show n
      | otherwise = show n
  Console.log
    (Array.intercalate
      "\n"
      (Array.mapWithIndex
        (\index s -> (if options.number then pad5 (index + 1) <> " " else "") <> s)
        (String.split (String.Pattern "\n") text)))
