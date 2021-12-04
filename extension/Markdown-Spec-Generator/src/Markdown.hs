{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Markdown
  ( grammarSeed
  , grammarName
  ) where

import Data.Char
import Data.Data          (Data)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict    (Map)
import Data.String
import Data.Text          (Text)
import Data.Typeable      (Typeable)
import GHC.Exts
import GHC.Generics

import Prelude            hiding (words)

import BNF
import LoremIpsum
import TopLevelDomain
import URI


grammarSeed :: Block
grammarSeed = minBound


grammarName :: String
grammarName = "markdown"


-- | Options for cell alignment in tables.
--
-- @since 0.0.4.0
data CellAlign
  = -- | No specific alignment specified
    CellAlignDefault
  | -- | Left-alignment
    CellAlignLeft
  | -- | Right-alignment
    CellAlignRight
  | -- | Center-alignment
    CellAlignCenter
  deriving stock (Show, Bounded, Enum, Eq, Ord, Data, Typeable, Generic)


newtype TableRow = TableRow (NonEmpty (NonEmpty Inline))
  deriving newtype (Show, Eq, Ord)
  deriving stock   (Data, Typeable, Generic)


-- | Inline markdown content.
data Inline
  = -- | Plain text
    Plain Text
  | -- | Line break (hard)
    LineBreak
  | -- | Emphasis
    Emphasis Bool (NonEmpty Inline)
  | -- | Strong emphasis
    Strong Bool (NonEmpty Inline)
  | -- | Strikeout
    Strikeout (NonEmpty Inline)
  | -- | Subscript
    Subscript (NonEmpty Inline)
  | -- | Superscript
    Superscript (NonEmpty Inline)
  | -- | Code span
    CodeSpan Text
  | -- | Link with text, destination, and optionally title
    Link (NonEmpty Inline) URI (Maybe Text)
  | -- | Image with description, URL, and optionally title
    Image (NonEmpty Inline) URI (Maybe Text)
  deriving stock (Show, Eq, Ord, Data, Typeable, Generic)


-- | We can think of (NonEmpty Inline) markdown document (NonEmpty Inline)s (NonEmpty Inline) collection of
-- blocks—structural elements like paragraphs, block quotations, lists,
-- headings, thematic breaks, (NonEmpty Inline)nd code blocks. Some blocks (like block
-- quotes (NonEmpty Inline)nd list items) contain other blocks; others (like headings (NonEmpty Inline)nd
-- paragraphs) contain inline content, see 'Inline'.
 --
-- We can divide blocks into two types: container blocks, which can contain
-- other blocks, and leaf blocks, which cannot.
data Block
  = -- | Thematic break, leaf block
    ThematicBreak Bool
  | -- | Heading (level 1), leaf block
    Heading1 (NonEmpty Inline)
  | -- | Heading (level 2), leaf block
    Heading2 (NonEmpty Inline)
  | -- | Heading (level 3), leaf block
    Heading3 (NonEmpty Inline)
  | -- | Heading (level 4), leaf block
    Heading4 (NonEmpty Inline)
  | -- | Heading (level 5), leaf block
    Heading5 (NonEmpty Inline)
  | -- | Heading (level 6), leaf block
    Heading6 (NonEmpty Inline)
  | -- | Code block, leaf block with info string (NonEmpty Inline)nd contents
    CodeBlock Text
  | -- | Naked content, without an enclosing tag
    Naked (NonEmpty Inline)
  | -- | Paragraph, leaf block
    Paragraph (NonEmpty Inline)
  | -- | Blockquote container block
    Blockquote [Block]
  | -- | Ordered list ('Word' is the start index), container block
    OrderedList   Word (NonEmpty [Block])
  | -- | Unordered list, container block
    UnorderedList Bool (NonEmpty [Block])
  | -- | Table, first (NonEmpty Inline)rgument is the (NonEmpty Inline)lignment options, then we have (NonEmpty Inline)
    -- 'NonEmpty' list of rows, where every row is (NonEmpty Inline) 'NonEmpty' list of
    -- cells, where every cell is (NonEmpty Inline)n @a@ thing.
    --
    -- The first row is (NonEmpty Inline)lways the header row, because pipe-tables that we
    -- support cannot lack (NonEmpty Inline) header row.
    --
    -- @since 0.0.4.0
    Table TableRow (NonEmpty CellAlign) (NonEmpty TableRow)
  deriving stock (Show, Eq, Ord, Data, Typeable, Generic)


instance Bounded Block where

    maxBound = Table minBound (pure minBound) $ pure minBound

    minBound = ThematicBreak False


instance Bounded Inline where

    maxBound = Image (pure minBound) minBound $ Just ""

    minBound = Plain ""


instance Bounded TableRow where

    maxBound = TableRow . pure $ pure minBound

    minBound = maxBound


instance Enum TableRow where

    toEnum   = const maxBound

    fromEnum = const 0


instance Enum Inline where

    toEnum x =
      let nei = pure minBound
          tlt = Just ""
      in  case x `modulusOf` (undefined :: Inline) of
            0  -> Plain ""
            1  -> LineBreak
            2  -> Emphasis False nei
            3  -> Emphasis True  nei
            4  -> Strong   False nei
            5  -> Strong   True  nei
            6  -> Strikeout      nei
            7  -> Subscript      nei
            8  -> Superscript    nei
            9  -> CodeSpan ""
            10 -> Link  nei minBound Nothing
            11 -> Link  nei minBound tlt
            12 -> Image nei minBound Nothing
            13 -> Image nei minBound tlt

    fromEnum =
      \case
         Plain          {} -> 0
         LineBreak         -> 1
         Emphasis False _  -> 2
         Emphasis True  _  -> 3
         Strong   False _  -> 4
         Strong   True  _  -> 5
         Strikeout      {} -> 6
         Subscript      {} -> 7
         Superscript    {} -> 8
         CodeSpan       {} -> 9
         Link  _ _ Nothing -> 10
         Link           {} -> 11
         Image _ _ Nothing -> 12
         Image          {} -> 13


instance Enum Block where

    toEnum x =
      let nei = pure minBound
          neb = pure minBound
          tlt = Just ""
      in  case x `modulusOf` (undefined :: Block) of
            0  -> minBound
            1  -> ThematicBreak True
            2  -> Heading1   nei
            3  -> Heading2   nei
            4  -> Heading3   nei
            5  -> Heading4   nei
            6  -> Heading5   nei
            7  -> Heading6   nei
            8  -> CodeBlock  ""
            9  -> Naked      nei
            10 -> Paragraph  nei
            11 -> Blockquote neb
            12 -> OrderedList   0     $ pure neb
            13 -> UnorderedList False $ pure neb
            14 -> UnorderedList True  $ pure neb
            15 -> maxBound

    fromEnum =
      \case
        ThematicBreak False   -> 0
        ThematicBreak True    -> 1
        Heading1           {} -> 2
        Heading2           {} -> 3
        Heading3           {} -> 4
        Heading4           {} -> 5
        Heading5           {} -> 6
        Heading6           {} -> 7
        CodeBlock          {} -> 8
        Naked              {} -> 9
        Paragraph          {} -> 10
        Blockquote         {} -> 11
        OrderedList        {} -> 12
        UnorderedList False _ -> 13
        UnorderedList True  _ -> 14
        Table              {} -> 15


instance HasNonTerminal CellAlign where

    nonTerminal = mkNonTerminal "CellAlign"


instance HasProductions CellAlign where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue CellAlign where

    ruleOfValue g x =
      let dashVal = ["-"] :: NonEmpty Terminal
          dashes  = note dashVal
          theRule = case x of
                      CellAlignDefault -> [      dashes ]
                      CellAlignLeft    -> [ ":", dashes ]
                      CellAlignRight   -> [      dashes, ":" ]
                      CellAlignCenter  -> [ ":", dashes, ":" ]
      in  ruleWithDeps g theRule [ dashVal ]


instance HasSuffixSymbol CellAlign where

    suffix = nonTerminal


instance IsGrammar CellAlign where

    grammarBNF = enumerableGrammar


instance HasNonTerminal Inline where

    nonTerminal = mkNonTerminal "Inline"


instance HasProductions Inline where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue Inline where

   ruleOfValue g x =
      let words  =  word `SepBy` space
          word   =  minBound  :: LoremIpsum
          parts  = [minBound] :: NonEmpty Inline
          uri    =  minBound  :: URI
          space  = " " :: Terminal
          spaces = space :| []
          quote  = "\'"
          ref    = (g, nonTerminal x)
          partsSurroundedBy :: [Symbol] -> (Rule, Dependencies)
          partsSurroundedBy y = ruleWithDeps ref (fromList $ y <> [ note parts ] <> y) [ parts ]
      in  case x of
            Plain          {} -> ruleWithDeps ref [ " ", note words, " " ] [ words ]
            LineBreak         -> ruleWithDeps ref [ tok space, note spaces, "\n" ] [ spaces ]
            Emphasis False  _ -> partsSurroundedBy [ "*" ]
            Emphasis True   _ -> partsSurroundedBy [ "_" ]
            Strong   False  _ -> partsSurroundedBy [ "*", "*" ]
            Strong   True   _ -> partsSurroundedBy [ "_", "_" ]
            Strikeout      {} -> partsSurroundedBy [ "~", "~" ]
            Subscript      {} -> partsSurroundedBy [ "~" ]
            Superscript    {} -> partsSurroundedBy [ "^" ]
            CodeSpan       {} -> ruleWithDeps ref [ "`", note words, "`" ] [ words ]
            Link  _ _ Nothing -> ruleWithDep2 ref [ "[", note parts , "]"
                                                , "(", note uri, ")"
                                                ] (parts, uri)
            Link  _ _ _       -> ruleWithDep3 ref [ "[", note parts , "]"
                                                , "(", note uri   , ")"
                                                , note spaces
                                                , quote, note words, quote
                                                ] (parts, uri, words)
            Image _ _ Nothing -> ruleWithDep2 ref [ "!", "[", note parts, "]"
                                                ,      "(", note uri  , ")"
                                                ] (parts, uri)
            Image _ _ _       -> ruleWithDep3 ref [ "!", "[", note parts, "]"
                                                ,      "(", note uri  , ")"
                                                , note spaces
                                                , quote, note words, quote
                                                ] (parts, uri, words)


instance HasSuffixSymbol Inline where

    suffix = nonTerminal


instance IsGrammar Inline where

    grammarBNF = enumerableGrammar


instance HasNonTerminal TableRow where

    nonTerminal = mkNonTerminal "TableRow"


instance HasProductions TableRow where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue TableRow where

    ruleOfValue g x =
      let words = minBound `SepBy` " "  :: SepBy LoremIpsum Terminal
          cells = cell     `SepBy` pipe :: SepBy (Padded (SepBy LoremIpsum Terminal)) Terminal
          cell  = Padded words
          pipe  = "|" :: Terminal
      in  ruleWithDeps g [ term pipe, note cells, term pipe, "\n" ] [ cells ]


instance HasSuffixSymbol TableRow where

    suffix = nonTerminal


instance IsGrammar TableRow where

    grammarBNF = enumerableGrammar


instance HasNonTerminal Block where

    nonTerminal = mkNonTerminal "Block"


instance HasProductions Block where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue Block where

    ruleOfValue g x =
      let words  = minBound `SepBy` space :: SepBy LoremIpsum Terminal
          parts  = Padded $ minBound `SepBy` space :: Padded (SepBy Inline Terminal)
          uri    =  minBound  :: URI
          eol    = "\n" :: Symbol
          space  = " "  :: Terminal
          spaces = space :| []
          header n =
            let prefix = fromString $ replicate n '#'
            in  ruleWithDeps g [ prefix, note parts, eol ] [ parts ]
          underlined x =
            let ne = [x] :: NonEmpty Terminal
            in  ruleWithDeps g [ term x, term x, note ne, eol ] [ ne ]
      in  case x of
            ThematicBreak False   -> underlined "="
            ThematicBreak True    -> underlined "-"
            Heading1           {} -> header 1
            Heading2           {} -> header 2
            Heading3           {} -> header 3
            Heading4           {} -> header 4
            Heading5           {} -> header 5
            Heading6           {} -> header 6
            CodeBlock          {} -> ruleWithDeps g [ "`", "`", "`", eol, note words, "`", "`", "`", eol ] [ words ]
            Naked              {} -> ruleWithDeps g [ note parts, eol ] [ parts ]
            Paragraph          {} -> ruleWithDeps g [ note parts, eol ] [ parts ]
            Blockquote         {} -> ruleWithDeps g [ ">", note parts, eol ] [ parts ]
            OrderedList        {} -> ruleWithDep2 g [ note spaces, "1", ".", note parts, eol ] (spaces, parts)
            UnorderedList False _ -> ruleWithDep2 g [ note spaces, "*", note parts, eol ] (spaces, parts)
            UnorderedList True  _ -> ruleWithDep2 g [ note spaces, "-", note parts, eol ] (spaces, parts)
            Table x y z           -> ruleWithDep3 g [ note x, note y, note z ] (x, y, z)


instance HasSuffixSymbol Block where

    suffix = nonTerminal


instance IsGrammar Block where

    grammarBNF = enumerableGrammar
