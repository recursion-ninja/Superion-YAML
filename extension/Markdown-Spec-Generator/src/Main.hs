{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           BNF                      (IsGrammar, toG4, finalizeGrammar, grammarBNF)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS 
import qualified Data.Text.IO as TX
import qualified Markdown                 as Markdown (grammarName, grammarSeed)
import           Prelude                  hiding (writeFile)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath.Posix    ((</>))

main :: IO ()
main = writeOutGrammarFormats Markdown.grammarSeed Markdown.grammarName


writeOutGrammarFormats :: IsGrammar p => p -> FilePath -> IO ()
writeOutGrammarFormats seed name =
    let file1 = "source.json"
        file2 = "source.g4"
        lang = finalizeGrammar $ grammarBNF seed
        json = encodePretty lang
        g4   = toG4 "Markdown" lang
    in  do  createDirectoryIfMissing True name
            BS.writeFile (name </> file1) json
            TX.writeFile (name </> file2) g4
