{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

module URI where

import Data.Data          (Data)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
import Data.Text          (Text)
import Data.Typeable      (Typeable)
import GHC.Exts
import GHC.Generics

import Prelude            hiding (words)

import BNF
import LoremIpsum
import TopLevelDomain


newtype URI = URI (Either URL Path)
  deriving newtype (Show, Eq, Ord)
  deriving stock   (Data, Typeable, Generic)


newtype URL = URL (Host, Maybe (NonEmpty Text), Maybe Fragment)
  deriving newtype (Show, Eq, Ord)
  deriving stock   (Data, Typeable, Generic)


newtype Path = Path (Slash, NonEmpty Text, Slash)
  deriving newtype (Show, Eq, Ord)
  deriving stock   (Data, Typeable, Generic)


newtype Host = Host (Maybe Text, Text, TopLevelDomain)
  deriving newtype (Show, Eq, Ord)
  deriving stock   (Data, Typeable, Generic)


-- | Fragament text without the @ # @.
newtype Fragment = Fragment Text
  deriving newtype (Show, Eq, Ord, IsString)
  deriving stock   (Data, Typeable, Generic)


newtype Slash = Slash Bool
  deriving newtype (Show, Enum, Eq, Ord)
  deriving stock   (Data, Typeable, Generic)


instance Bounded Fragment where

    maxBound = Fragment ""

    minBound = Fragment ""


instance Bounded Host where

    maxBound = Host (Just "", "", minBound)

    minBound = Host (Nothing, "", minBound)


instance Bounded Path where

    maxBound = Path (toEnum 1, pure "", toEnum 1)

    minBound = Path (toEnum 0, pure "", toEnum 0)


instance Bounded URI where

    maxBound = URI $ Right minBound

    minBound = URI $ Left  minBound


instance Bounded URL where

    maxBound = URL (minBound, Just $ pure "", Just "")

    minBound = URL (minBound, Nothing, Nothing)




instance Enum Fragment where

    toEnum   = const minBound

    fromEnum = const 0


instance Enum Host where

    toEnum x
      | even x    = minBound
      | otherwise = maxBound

    fromEnum (Host (Nothing, _, _)) = 0
    fromEnum (Host (Just  _, _, _)) = 1


instance Enum Path where

    toEnum x =
      let txt = pure ""
      in  case x `modulusOf` (undefined :: Path) of
            0 -> minBound
            1 -> Path (toEnum 0, txt, toEnum 1)
            2 -> Path (toEnum 1, txt, toEnum 0)
            _ -> maxBound

    fromEnum (Path (x, _, y)) = fromEnum x + fromEnum y


instance Enum URI where

    toEnum x
      | even x    = minBound
      | otherwise = maxBound

    fromEnum (URI Left  {}) = 0
    fromEnum (URI Right {}) = 1


instance Enum URL where

    toEnum x =
      let txt = Just $ pure ""
      in  case x `modulusOf` (undefined :: URL) of
            0 -> minBound
            1 -> URL (minBound, Nothing, Just "")
            2 -> URL (minBound,     txt, Nothing)
            _ -> maxBound

    fromEnum (URL (_, Just _, Just  _)) = 3
    fromEnum (URL (_, Just _, Nothing)) = 2
    fromEnum (URL (_, Nothing, Just _)) = 1
    fromEnum _                          = 0




instance HasNonTerminal Fragment where

    nonTerminal = mkNonTerminal "Fragment"


instance HasNonTerminal Host where

    nonTerminal = mkNonTerminal "Host"


instance HasNonTerminal Path where

    nonTerminal = mkNonTerminal "Path"


instance HasNonTerminal URI where

    nonTerminal = mkNonTerminal "URI"


instance HasNonTerminal URL where

    nonTerminal = mkNonTerminal "URL"




instance HasSuffixSymbol Fragment where

    suffix = nonTerminal


instance HasSuffixSymbol Host where

    suffix = nonTerminal


instance HasSuffixSymbol Path where

    suffix = nonTerminal


instance HasSuffixSymbol URI where

    suffix = nonTerminal


instance HasSuffixSymbol URL where

    suffix = nonTerminal




instance HasProductions Fragment where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasProductions Host where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasProductions Path where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasProductions URI where

    productionRule g x = enumerableProductions g (nonTerminal x) x

instance HasProductions URL where

    productionRule g x = enumerableProductions g (nonTerminal x) x




instance IsGrammar Fragment where

    grammarBNF = enumerableGrammar


instance IsGrammar Host where

    grammarBNF = enumerableGrammar


instance IsGrammar URL where

    grammarBNF = enumerableGrammar


instance IsGrammar Path where

    grammarBNF = enumerableGrammar


instance IsGrammar URI where

    grammarBNF = enumerableGrammar




instance HasRuleByValue Fragment where

    ruleOfValue g =
      let wlog = minBound :: LoremIpsum
      in  const $ ruleWithDeps g [ "#", note wlog ] [ wlog ]


instance HasRuleByValue Host where

    ruleOfValue g x =
      let dom = minBound :: LoremIpsum
          tld = minBound :: TopLevelDomain
          sub = minBound :: LoremIpsum
      in  case fromEnum x of
            0 -> ruleWithDep2 g [ "https://",                note dom, ".", note tld ] (dom, tld)
            _ -> ruleWithDep2 g [ "https://", note sub, ".", note dom, ".", note tld ] (dom, tld)


instance HasRuleByValue Path where

    ruleOfValue g x =
      let part = minBound :: LoremIpsum
          dash = "/" :: Terminal
          path = part `SepBy` dash
          rule = case fromEnum x of
                   0 -> [      note path      ]
                   1 -> [      note path, "/" ]
                   2 -> [ "/", note path      ]
                   _ -> [ "/", note path, "/" ]
      in  ruleWithDeps g rule [ path ]


instance HasRuleByValue URI where

    ruleOfValue g (URI (Left  x)) = ruleWithDeps g [ note x ] [ x ]
    ruleOfValue g (URI (Right y)) = ruleWithDeps g [ note y ] [ y ]


instance HasRuleByValue URL where

   ruleOfValue g x =
      let host  = minBound :: Host
          seg   = minBound :: LoremIpsum
          frag  = minBound :: Fragment
          dash  = "/" :: Terminal
          route = seg `SepBy` dash
      in  case fromEnum x of
            0 -> ruleWithDeps g [ note host                             ] [host]
            1 -> ruleWithDep2 g [ note host,                  note frag ] (host,        frag)
            2 -> ruleWithDep2 g [ note host, "/", note route            ] (host, route      )
            _ -> ruleWithDep3 g [ note host, "/", note route, note frag ] (host, route, frag)
