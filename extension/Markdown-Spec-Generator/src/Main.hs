module Main where

import           BNF                      (finalizeGrammar, grammarBNF)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.ByteString.Lazy     (writeFile)
import qualified Markdown                 as Markdown (grammarName, grammarSeed)
import           Prelude                  hiding (writeFile)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath.Posix    ((</>))

main :: IO ()
main = writeOutGrammarFormats Markdown.grammarSeed Markdown.grammarName


writeOutGrammarFormats seed name =
    let file = "source.json"
        path = name </> file
        lang = finalizeGrammar $ grammarBNF seed
        json = encodePretty lang
    in  do  createDirectoryIfMissing True name
            writeFile path json
