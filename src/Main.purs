module Main where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
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

type Options =
  { files :: Array String
  , number :: Boolean
  , numberNonblank :: Boolean
  , squeezeBlank :: Boolean
  }

readOptions :: Array String -> Options
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
          "--number-nonblank" -> Record.merge { numberNonblank: true } o
          "--squeeze-blank" -> Record.merge { squeezeBlank: true } o
          _ -> o
      )
      { number: false, numberNonblank: false, squeezeBlank: false }
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
    (_.s
      (Array.foldl
        (\{ b, n, s } l ->
          let isBlank = String.null l
          in
            if options.squeezeBlank && b && isBlank
            then { b, n, s }
            else
              let
                line =
                  ( if options.numberNonblank
                    then (if isBlank then "     " else pad5 n) <> " "
                    else if options.number then pad5 n <> " "
                    else ""
                  ) <> l
                number = if options.numberNonblank && isBlank then n else n + 1
              in { b: isBlank, n: number, s: (s <> "\n" <> line) })
        { b: false, n: 1, s: "" }
        (String.split (String.Pattern "\n") text)))
