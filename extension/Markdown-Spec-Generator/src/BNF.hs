{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module BNF where

import Data.Aeson.Types
import Data.Bifunctor
import Data.Char          (toUpper)
import Data.Coerce
import Data.Foldable      hiding (toList)
import Data.Ord
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict    (Map, insert, keysSet, unionWith)
import Data.Sequence      (Seq)
import Data.Set           (Set, filter, member)
import GHC.Exts           (IsList(..))
import Data.String
import Data.Text          (Text, intercalate, map, unpack)
import Numeric            (showHex)
import Prelude            hiding (filter, map)


newtype Grammar = Grammar (Map NonTerminal (Set Rule))


newtype GrammarBuilder = GB { builder :: Grammar -> Grammar }


newtype Production = Production { extractProduction :: (NonTerminal, Set Rule) }


newtype Dependencies = Deps { getDependencies :: Maybe (Set Production) }
    deriving newtype (Eq, Ord, Monoid, Semigroup)


newtype Rule = Rule (Seq Symbol)
    deriving newtype (Eq, Ord, Show)


newtype Symbol = Symbol (Either NonTerminal Terminal)
    deriving newtype (Eq, Ord)


newtype Terminal = Terminal (Either Char Text)
    deriving newtype (Eq, Ord)


newtype NonTerminal = NonTerminal Text
    deriving newtype (Eq, IsString, Ord)


newtype Padded a = Padded a
    deriving stock (Eq, Ord, Show)


data SepBy a b = SepBy a b
    deriving stock (Eq, Ord, Show)


data a :<>: b = a :<>: b


class IsSymbol a where

    tok :: a -> Symbol


class IsGrammar a where

    grammarBNF :: a -> GrammarBuilder


class IsDefinedBy a where

    alreadyDefinedBy :: NonTerminal -> a -> Bool


class HasNonTerminal a => HasProductions a where

    productionRule :: forall b. IsDefinedBy b => b -> a -> (Production, Dependencies)


class HasRuleByValue a where

    ruleOfValue :: forall b. IsDefinedBy b => b -> a -> (Rule, Dependencies)


class HasNonTerminal a where

    nonTerminal :: a -> NonTerminal


class HasSuffixSymbol a where

    suffix :: a -> NonTerminal


class HasTerminal a where

    terminal :: a -> Terminal


--newtype Grammar = Grammar (Map NonTerminal (Set Rule))

instance IsDefinedBy Grammar where

    alreadyDefinedBy nt (Grammar m) = nt `member` keysSet m


instance IsDefinedBy Dependencies where

    alreadyDefinedBy _  (Deps  Nothing)  = False
    alreadyDefinedBy nt (Deps (Just ds)) = nt `alreadyDefinedBy` ds


instance HasNonTerminal a => IsDefinedBy (NonEmpty a) where

    alreadyDefinedBy nt = (nt `elem`) . fmap nonTerminal


instance HasNonTerminal a => IsDefinedBy [a] where

    alreadyDefinedBy nt = (nt `elem`) . fmap nonTerminal


instance (HasNonTerminal a, Ord a) => IsDefinedBy (Set a) where

    alreadyDefinedBy nt set = nt `elem` (nonTerminal <$> toList set)


instance IsDefinedBy NonTerminal where

    alreadyDefinedBy = (==)


instance (IsDefinedBy a, IsDefinedBy b) => IsDefinedBy (a,b) where

    alreadyDefinedBy nt (a,b) = nt `alreadyDefinedBy` a || nt `alreadyDefinedBy` b


instance ToJSON Grammar where

    toJSON = object . fmap buildProduction . toList
      where
        buildProduction :: Production -> (Key, Value)
        buildProduction =
          let toObjKey = fromString . unpack . coerce
              toObjVal = toJSONList . toList
          in  bimap toObjKey toObjVal . extractProduction


instance ToJSON Rule where

    toJSON = String . intercalate " " . go . toList
      where
        go                                          []  = []
        go (                    (Symbol (Left non)):xs) = renderNonTerminal non : go xs
        go ((Symbol (Right t1)):(Symbol (Right t2)):xs) = go $ joinTerminals t1 t2 : xs
        go (                    (Symbol (Right t )):xs) = encloseTerminal t : go xs

        renderNonTerminal = map toUpper . coerce

        renderTerminal (Terminal (Left    c)) = fromString [c]
        renderTerminal (Terminal (Right txt)) = txt

        encloseTerminal x = "'" <> renderTerminal x <> "'"

        joinTerminals t1 t2 = Symbol . Right . Terminal . Right $ renderTerminal t1 <> renderTerminal t2


instance Eq Production where

    (==) x y =
      let f = fst . extractProduction
      in  f x == f y


instance Ord Production where

    compare = comparing $ fst . extractProduction


instance Show Symbol where

    show (Symbol (Left  nonTerm)) = show nonTerm
    show (Symbol (Right litTerm)) =
      let Terminal v = litTerm
      in  case v of
            Left  c -> show c
            Right t -> "'" <> toList t <> "'"


instance Show NonTerminal where

    show = toList . (coerce :: NonTerminal -> Text)


instance Show Production where

    show (Production (key, set)) = unlines $ begin : rules
      where
        begin = show key <> " -->"
        line  = ("  | " <>) . unwords . fmap show . toList
        rules = line <$> toList set


instance Show Grammar where

    show = unlines . ("Grammar:":) . fmap (unlines . fmap ("    "<>) . lines . show) . toList


instance HasSuffixSymbol a => HasNonTerminal (NonEmpty a) where

    nonTerminal (x:|_) = "ONEORMORE" `appendSuffix` x


instance HasSuffixSymbol a  => HasNonTerminal (Padded a) where

    nonTerminal (Padded a) = "PADDED" `appendSuffix` a


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasNonTerminal (SepBy a b) where

    nonTerminal (a `SepBy` b) = ("SEPBY" `appendSuffix` a) `appendSuffix` b


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasNonTerminal (a :<>: b) where

    nonTerminal (a :<>: b) = (suffix a <> "PAIRED") `appendSuffix` b


instance HasNonTerminal NonTerminal where

    nonTerminal = id


instance HasNonTerminal Production where

    nonTerminal (Production (x,_)) = x


instance {-# OVERLAPPABLE #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (NonEmpty a) where

    productionRule g ne@(x:|_) =
      let elemRef = note x
          selfRef = nonTerminal ne
      in  fromRulesWithDeps g selfRef
            [ [ elemRef ]
            , [ elemRef, tok selfRef ]
            ]
            [ x ]


instance {-# OVERLAPPING #-} HasProductions (NonEmpty Terminal) where

    productionRule _ ne@(x:|_) = fromRulesAndNoDeps (nonTerminal ne)
            [ [ term x ]
            , [ term x, note ne ]
            ]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (Padded a) where

    productionRule g p@(Padded a) =
      let spaces = [" "] :: NonEmpty Terminal
      in  fromRulesWithDeps2 g (nonTerminal p)
            [ [              note a ]
            , [ note spaces, note a ]
            , [              note a, note spaces ]
            , [ note spaces, note a, note spaces ]
            ]
            (a, spaces)


instance {-# OVERLAPPING #-} HasProductions (Padded Terminal) where

    productionRule g p@(Padded a) =
      let spaces = [" "] :: NonEmpty Terminal
      in  fromRulesWithDeps g (nonTerminal p)
            [ [              term a ]
            , [ note spaces, term a ]
            , [              term a, note spaces ]
            , [ note spaces, term a, note spaces ]
            ]
            [spaces]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasProductions b, HasSuffixSymbol a, HasSuffixSymbol b) => HasProductions (SepBy a b) where

    productionRule g s@(a `SepBy` b) =
      let ne = (b :<>: a) :| []
      in  fromRulesWithDeps2 g (nonTerminal s)
            [ [ note a ]
            , [ note ne ]
            ]
            (a, ne)


instance {-# OVERLAPPING #-} (HasProductions b, HasSuffixSymbol b) => HasProductions (SepBy Terminal b) where

    productionRule g s@(t `SepBy` b) =
      let ne = (b :<>: t) :| []
      in  fromRulesWithDeps g (nonTerminal s)
            [ [ term t  ]
            , [ note ne ]
            ]
            [ ne ]


instance {-# OVERLAPPING #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (SepBy a Terminal) where

    productionRule g s@(a `SepBy` t) =
      let ne = (t :<>: a) :| []
      in  fromRulesWithDeps g (nonTerminal s)
            [ [ note a  ]
            , [ note ne ]
            ]
            [ ne ]


instance {-# OVERLAPPING #-} HasProductions (SepBy Terminal Terminal) where

    productionRule g s@(t1 `SepBy` t2) =
      let ne = (t2 :<>: t1) :| []
      in  fromRulesWithDeps g (nonTerminal s)
            [ [ term t1  ]
            , [ note ne ]
            ]
            [ ne ]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasProductions b, HasSuffixSymbol a, HasSuffixSymbol b) => HasProductions (a :<>: b) where

    productionRule g j@(a :<>: b) = fromRulesWithDeps2 g (nonTerminal j)
        [ [ note a, note b ]
        ]
        (a, b)


instance {-# OVERLAPPING #-} (HasProductions b, HasSuffixSymbol b) => HasProductions (Terminal :<>: b) where

    productionRule g j@(t :<>: b) = fromRulesWithDeps g (nonTerminal j)
        [ [ term t, note b ]
        ]
        [ b ]


instance {-# OVERLAPPING #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (a :<>: Terminal) where

    productionRule g j@(a :<>: t) = fromRulesWithDeps g (nonTerminal j)
        [ [ note a, term t ]
        ]
        [ a ]


instance {-# OVERLAPPING #-} HasProductions (Terminal :<>: Terminal) where

    productionRule _ j@(t1 :<>: t2) = fromRulesAndNoDeps (nonTerminal j)
        [ [ term t1, term t2 ]
        ]


instance HasProductions Production where

    productionRule _ x = (x, mempty)


instance HasSuffixSymbol a => HasSuffixSymbol (Padded a) where

    suffix = nonTerminal


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasSuffixSymbol (SepBy a b) where

    suffix = nonTerminal


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasSuffixSymbol (a :<>: b) where

    suffix = nonTerminal


instance HasSuffixSymbol Terminal where

    suffix (Terminal e) =
      let suff = case e of
                   Left  c   -> fromString $ "C_" <> (toUpper <$> showHex (fromEnum c) "")
                   Right txt -> "TERM_" <> txt
      in  NonTerminal suff


instance HasSuffixSymbol NonTerminal where

    suffix = id


instance HasTerminal Terminal where

    terminal = id








instance IsList Grammar where

    type (Item Grammar) = Production

    fromList = Grammar . fromList . fmap extractProduction

    toList (Grammar x) = Production <$> toList x


instance IsList Rule where

    type (Item Rule) = Symbol

    fromList = Rule . fromList

    toList (Rule x) = toList x


instance IsString Terminal where

    fromString  [] = Terminal $ Right ""
    fromString [x] = Terminal $ Left x
    fromString  xs = Terminal . Right $ fromString xs


instance IsString Symbol where

    fromString = Symbol . Right . fromString


instance IsSymbol Terminal where

    tok = Symbol . Right


instance IsSymbol NonTerminal where

    tok = Symbol . Left


instance Semigroup Grammar where

    (<>) (Grammar a) (Grammar b) = Grammar $ unionWith (<>) a b


instance Semigroup GrammarBuilder where

    (<>) (GB f) (GB g) = GB $ f . g


instance Semigroup NonTerminal where

    (<>) (NonTerminal x) (NonTerminal y) = NonTerminal $ x <> "_" <> y


instance (HasSuffixSymbol a, HasProductions a) => IsGrammar (NonEmpty a) where

    grammarBNF = GB . gBuild . (:[])


finalizeGrammar :: GrammarBuilder -> Grammar
finalizeGrammar = ($ Grammar mempty) . builder


appendSuffix :: HasSuffixSymbol a => NonTerminal -> a -> NonTerminal
appendSuffix prefix = (prefix <>) . suffix


note :: HasNonTerminal a => a -> Symbol
note = Symbol . Left . nonTerminal


term :: HasTerminal a => a -> Symbol
term = Symbol . Right . terminal


ruleNadaDeps :: Rule -> (Rule, Dependencies)
ruleNadaDeps r = (r, mempty)


{-# Specialise ruleWithDeps :: IsDefinedBy g => g -> Rule -> NonEmpty (NonEmpty Terminal) -> (Rule, Dependencies) #-}
ruleWithDeps :: (HasProductions a, IsDefinedBy g) => g -> Rule -> NonEmpty a -> (Rule, Dependencies)
ruleWithDeps g r = (,) <$> const r <*> dependencyN g


ruleWithDep2
  :: ( HasProductions a
     , HasProductions b
     , IsDefinedBy g
     )
  => g -> Rule -> (a, b) -> (Rule, Dependencies)
ruleWithDep2 g r = (,) <$> const r <*> dependency2 g


ruleWithDep3
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , IsDefinedBy g
     )
  => g -> Rule -> (a, b, c) -> (Rule, Dependencies)
ruleWithDep3 g r = (,) <$> const r <*> dependency3 g


fromRulesAndNoDeps :: NonTerminal -> Set Rule -> (Production, Dependencies)
fromRulesAndNoDeps t rs = (Production (t, rs), mempty)


fromRulesWithDeps :: (HasProductions a, IsDefinedBy b) => b -> NonTerminal -> Set Rule -> NonEmpty a -> (Production, Dependencies)
fromRulesWithDeps g t rs = (,) <$> const (Production (t, rs)) <*> dependencyN g


fromRulesWithDeps2
  :: ( HasProductions a
     , HasProductions b
     , IsDefinedBy g
     )
  => g -> NonTerminal -> Set Rule -> (a, b) -> (Production, Dependencies)
fromRulesWithDeps2 g t rs = (,) <$> const (Production (t, rs)) <*> dependency2 g


fromRulesWithDeps3
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , IsDefinedBy g
     )
  => g -> NonTerminal -> Set Rule -> (a, b, c) -> (Production, Dependencies)
fromRulesWithDeps3 g t rs = (,) <$> const (Production (t, rs)) <*> dependency3 g


dependency2
  :: ( HasProductions a
     , HasProductions b
     , IsDefinedBy g
     )
  => g
  -> (a, b)
  -> Dependencies
dependency2 g (a, b) =
    applyProduction g b $ applyProduction g a mempty


dependency3
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , IsDefinedBy g
     )
  => g
  -> (a, b, c)
  -> Dependencies
dependency3 g (a, b, c) = applyProduction g c $ dependency2 g (a, b)


dependency4
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , HasProductions d
     , IsDefinedBy g
     )
  => g
  -> (a, b, c, d)
  -> Dependencies
dependency4 g (a, b, c, d) = applyProduction g d $ dependency3 g (a, b, c)


dependencyN :: (HasProductions a, IsDefinedBy g) => g -> NonEmpty a -> Dependencies
dependencyN x = foldr (applyProduction x) mempty . toList


applyProduction :: (IsDefinedBy a, HasProductions b) => a -> b -> Dependencies -> Dependencies
applyProduction g val deps
  | nonTerminal val `alreadyDefinedBy` (g,deps) = deps
  | otherwise =
    let (p,d) = productionRule (g, deps) val
        d' = Deps $ Just [p]
    in  d' <> d <> deps


enumerableValuesOf
  :: ( Bounded a
     , Enum a
     )
  => a
  -> [a]
enumerableValuesOf input = tail $ [input] <> [minBound .. maxBound]


enumerableProductions
  :: ( Bounded a
     , Enum a
     , HasRuleByValue a
     , IsDefinedBy g
     )
  => g
  -> NonTerminal
  -> a
  -> (Production, Dependencies)
enumerableProductions g label =
    bimap (curry Production label . fromList) fold . unzip . fmap (ruleOfValue g) . enumerableValuesOf


enumerableGrammar :: HasProductions a => a -> GrammarBuilder
enumerableGrammar = GB . gBuild . (:|[])


gBuild :: (Foldable f, HasProductions a) => f a -> Grammar -> Grammar
gBuild xs grammar =
    let (deps, grammar') = foldl' define (mempty, grammar) xs
    in  case grammar' `definitionsRemovedFrom` deps of
          Deps Nothing    -> grammar'
          Deps (Just set) -> gBuild set grammar


define :: HasProductions a => (Dependencies, Grammar) -> a -> (Dependencies, Grammar)
define acc@(more, g@(Grammar m)) v
      | nonTerminal v `alreadyDefinedBy` g = acc
      | otherwise =
         let (new, deps) = review g v
         in  (more <> deps, Grammar $ m `append` new)


review :: (HasProductions a, IsDefinedBy b) => b -> a -> ((NonTerminal, Set Rule), Dependencies)
review g = bimap extractProduction id . productionRule g


append :: Map NonTerminal b -> (NonTerminal, b) -> Map NonTerminal b
append = flip $ uncurry insert


definitionsRemovedFrom :: Grammar -> Dependencies -> Dependencies
definitionsRemovedFrom grammar =
  \case
    Deps Nothing    -> mempty
    Deps (Just set) ->
      let -- not . (`alreadyDefinedBy` grammar)
          pruned = filter ((not . (`alreadyDefinedBy` grammar)) . nonTerminal) set
      in  if   null pruned
          then mempty
          else Deps $ Just pruned
