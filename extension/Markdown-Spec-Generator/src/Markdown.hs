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


grammarSeed :: Block
grammarSeed = minBound


grammarName :: String
grammarName = "markdown"


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


instance Bounded Fragment where

    maxBound = Fragment ""

    minBound = Fragment ""


instance Bounded Host where

    maxBound = Host (Just "", "", minBound)

    minBound = Host (Nothing, "", minBound)


instance Bounded Inline where

    maxBound = Image (pure minBound) minBound $ Just ""

    minBound = Plain ""


instance Bounded Path where

    maxBound = Path (toEnum 1, pure "", toEnum 1)

    minBound = Path (toEnum 0, pure "", toEnum 0)


instance Bounded URI where

    maxBound = URI $ Left  minBound

    minBound = URI $ Right minBound


instance Bounded URL where

    maxBound = URL (minBound, Just $ pure "", Just "")

    minBound = URL (minBound, Nothing, Nothing)


instance Bounded TableRow where

    maxBound = TableRow . pure $ pure minBound

    minBound = maxBound


instance Enum Fragment where

    toEnum   = const minBound

    fromEnum = const 0


instance Enum Host where

    toEnum x
      | even x    = minBound
      | otherwise = maxBound

    fromEnum (Host (Nothing, _, _)) = 0
    fromEnum (Host (Just  _, _, _)) = 1


instance Enum URI where

    toEnum x
      | even x    = minBound
      | otherwise = maxBound

    fromEnum (URI Left  {}) = 0
    fromEnum (URI Right {}) = 1


instance Enum TableRow where

    toEnum   = const maxBound

    fromEnum = const 0


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


instance Enum Path where

    toEnum x =
      let txt = pure ""
      in  case x `modulusOf` (undefined :: Path) of
            0 -> minBound
            1 -> Path (toEnum 0, txt, toEnum 1)
            2 -> Path (toEnum 1, txt, toEnum 0)
            _ -> maxBound

    fromEnum (Path (x, _, y)) = fromEnum x + fromEnum y


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


{-
instance HasSymbol (Block a) where

    symbol = "BLOCK"


instance HasSymbol CellAlign where

    symbol = "CELLALIGN"


instance HasSymbol Inline where

    symbol = "INLINE"


instance HasSymbol a => HasProductions (Block a) where

    productions =
        [ ThematicBreak
        , (Heading1 a)
        , (Heading2 a)
        , (Heading3 a)
        , (Heading4 a)
        , (Heading5 a)
        , (Heading6 a)
        , (CodeBlock (Maybe Text) Text)
        , (Naked a)
        , (Paragraph a)
        , (Blockquote [Block a])
        , (OrderedList Word (NonEmpty [Block a])))
        , (UnorderedList (NonEmpty [Block a]))
        , (Table (NonEmpty CellAlign) (NonEmpty (NonEmpty a))
        ]
    productionRule ThematicBreak  = ['--', ]
    productionRule ThematicBreakX = [['-', symbol ThematicBreakX], "-"]
    productionRule (Heading1 a)
    productionRule (Heading2 a)
    productionRule (Heading3 a)
    productionRule (Heading4 a)
    productionRule (Heading5 a)
    productionRule (Heading6 a)
    productionRule (CodeBlock (Maybe Text) Text)
    productionRule (Naked a)
    productionRule (Paragraph a)
    productionRule (Blockquote [Block a])
    productionRule (OrderedList Word (NonEmpty [Block a])))
    productionRule (UnorderedList (NonEmpty [Block a]))
    productionRule (Table (NonEmpty CellAlign) (NonEmpty (NonEmpty a)))
-}


{-
instance HasNonTerminal a where

    nonTerminal :: a -> NonTerminal


instance HasNonTerminal a => HasProductions a where

    productionRule :: a -> Production


instance HasSuffixSymbol a where

    suffix :: a -> Symbol


instance IsGrammar a where

    grammarBNF :: a -> GrammarBuilder
-}




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


instance HasNonTerminal Fragment where

    nonTerminal = mkNonTerminal "Fragment"


instance HasProductions Fragment where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue Fragment where

    ruleOfValue g =
      let wlog = minBound :: LoremIpsum
      in  const $ ruleWithDeps g [ "#", note wlog ] [ wlog ]


instance HasSuffixSymbol Fragment where

    suffix = nonTerminal


instance IsGrammar Fragment where

    grammarBNF = enumerableGrammar


instance HasNonTerminal Host where

    nonTerminal = mkNonTerminal "Host"


instance HasProductions Host where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue Host where

    ruleOfValue g x =
      let dom = minBound :: LoremIpsum
          tld = minBound :: TopLevelDomain
          sub = minBound :: LoremIpsum
      in  case fromEnum x of
            0 -> ruleWithDep2 g [ "https://",                note dom, ".", note tld ] (dom, tld)
            _ -> ruleWithDep2 g [ "https://", note sub, ".", note dom, ".", note tld ] (dom, tld)


instance HasSuffixSymbol Host where

    suffix = nonTerminal


instance IsGrammar Host where

    grammarBNF = enumerableGrammar


instance HasNonTerminal URL where

    nonTerminal = mkNonTerminal "URL"


instance HasProductions URL where

    productionRule g x = enumerableProductions g (nonTerminal x) x


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


instance HasSuffixSymbol URL where

    suffix = nonTerminal


instance IsGrammar URL where

    grammarBNF = enumerableGrammar


instance HasNonTerminal Path where

    nonTerminal = mkNonTerminal "Path"


instance HasProductions Path where

    productionRule g x = enumerableProductions g (nonTerminal x) x


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


instance HasSuffixSymbol Path where

    suffix = nonTerminal


instance IsGrammar Path where

    grammarBNF = enumerableGrammar


instance HasNonTerminal URI where

    nonTerminal = mkNonTerminal "URI"


instance HasProductions URI where

    productionRule g x = enumerableProductions g (nonTerminal x) x


instance HasRuleByValue URI where

    ruleOfValue g (URI (Left  x)) = ruleWithDeps g [ note x ] [ x ]
    ruleOfValue g (URI (Right y)) = ruleWithDeps g [ note y ] [ y ]


instance HasSuffixSymbol URI where

    suffix = nonTerminal


instance IsGrammar URI where

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


modulusOf
  :: ( Bounded a
     , Enum a
     )
  => Int
  -> a
  -> Word
modulusOf n e =
   let m = succ . fromEnum . last $ [e] <> [maxBound]
   in  toEnum $ n `mod` m


mkNonTerminal = const . fromString . fmap toUpper
