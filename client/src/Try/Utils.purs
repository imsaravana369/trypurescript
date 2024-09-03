module Try.Utils where

import Data.Array.NonEmpty
import Data.Either
import Data.Maybe
import Foreign.Generic.Class
import Prelude
import PureScript.CST
import PureScript.CST.Errors

import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Try.CSTBuilder as CSTBuilder

toParsedTree :: String -> Either String CSTBuilder.Tree
toParsedTree code = case parseModule code of
      ParseSucceeded cst -> lmap (const $ "shouldn't happen: decode failed") $ runExcept do
                                let cstJson = encode cst
                                decode cstJson
      ParseSucceededWithErrors _ err -> Left $ foldMap1 renderError err

      ParseFailed err -> Left $ renderError err

    where 
      renderError { error, position} = printParseError error <> " " <> (show position) <> "\n"
