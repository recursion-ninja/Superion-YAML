{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module BNF
  ( IsSymbol(..)
  , IsGrammar(..)
  , IsDefinedBy(..)
  , HasNonTerminal(..)
  , HasRuleByValue(..)
  , HasSuffixSymbol(..)
  , HasTerminal(..)
  , HasProductions(..)
  , ToG4(..)
  , getGrammarPairs
  , modulusOf
  , mkNonTerminal
  , enumerableGrammar
  , enumerableProductions
  , ruleNadaDeps
  , ruleWithDeps
  , ruleWithDep2
  , ruleWithDep3
  , ruleWithDep4
  , fromRulesAndNoDeps
  , fromRulesWithDeps
  , fromRulesWithDeps2
  , fromRulesWithDeps3
  , fromRulesWithDeps4
  , term
  , note
  , finalizeGrammar
  , Grammar()
  , GrammarBuilder(..)
  , Production(..)
  , Dependencies()
  , Rule()
  , Symbol()
  , Terminal(..)
  , NonTerminal(..)
  , Padded(..)
  , SepBy(..)
  , (:<>:)(..)
  , (:||:)(..)
  , getRulesAndDeps
  , appendSymbols
  , appendSymbolsToSet
  ) where



import Data.Aeson.Types
import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.Foldable      hiding (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict    (Map, (!?), insert, keysSet, unionWith)
import Data.Ord
import Data.Sequence      (Seq)
import Data.Set           (Set, filter, member)
import qualified Data.Set           as Set
import Data.String
import Data.Text          (Text, intercalate, map, unpack)
import GHC.Exts           (IsList(..))
import Numeric            (showHex)
import Prelude            hiding (filter, map)

import Debug.Trace


newtype Grammar = Grammar (Map NonTerminal (Set Rule))

getGrammarPairs :: Grammar -> [(NonTerminal, [Rule])]
getGrammarPairs (Grammar m) = fmap (fmap toList) $ toList m


newtype GrammarBuilder = GB { builder :: Grammar -> Grammar }


newtype Production = Production { extractProduction :: (NonTerminal, Set Rule) }


newtype Dependencies = Deps ( Maybe (Set Production) )
    deriving newtype (Eq, Ord, Monoid, Semigroup)


newtype Rule = Rule (Terminal, Seq Symbol)
    deriving newtype (Eq, Ord, Show)


newtype Symbol = Symbol (Either NonTerminal Terminal)
    deriving newtype (Eq, Ord)


newtype Terminal = Terminal (Either Char Text)
    deriving newtype (Eq, Ord, Show)


newtype NonTerminal = NonTerminal Text
    deriving newtype (Eq, Ord)


newtype Padded a = Padded a
    deriving stock (Eq, Ord, Show)


data SepBy a b = SepBy a b
    deriving stock (Eq, Ord, Show)


data a :<>: b = a :<>: b


data a :||: b = a :||: b


class IsSymbol a where

    tok :: a -> Symbol


class IsGrammar a where

    grammarBNF :: a -> GrammarBuilder


class IsDefinedBy a where

    alreadyDefinedBy :: NonTerminal -> a -> Bool


class IsDefinedBy a => IsQueryable a where

    queryRules :: NonTerminal -> a -> Maybe (Set Rule)


class HasNonTerminal a => HasProductions a where

    productionRule :: forall b. IsQueryable b => b -> a -> (Production, Dependencies)


class HasRuleByValue a where

    ruleOfValue :: forall b. IsQueryable b => b -> a -> (Rule, Dependencies)


class HasNonTerminal a where

    nonTerminal :: a -> NonTerminal


class HasSuffixSymbol a where

    suffix :: a -> NonTerminal


class HasTerminal a where

    terminal :: a -> Terminal


class ToG4 a where

    toG4 :: Text -> a -> Text


--newtype Grammar = Grammar (Map NonTerminal (Set Rule))

instance IsDefinedBy Grammar where

    alreadyDefinedBy nt (Grammar m) = nt `member` keysSet m


instance IsQueryable Grammar where

    queryRules nt (Grammar m) = m !? nt


instance IsDefinedBy Dependencies where

    alreadyDefinedBy _  (Deps  Nothing)  = False
    alreadyDefinedBy nt (Deps (Just ds)) = nt `alreadyDefinedBy` ds


instance IsQueryable Dependencies where

    queryRules _  (Deps  Nothing)  = Nothing
    queryRules nt (Deps (Just ds)) =
        let m = fromList $ extractProduction <$> toList ds
        in  m !? nt


instance IsDefinedBy NonTerminal where

    alreadyDefinedBy = (==)


instance IsDefinedBy Production where

    alreadyDefinedBy nt = (nt ==) . fst . extractProduction


instance IsQueryable Production where

    queryRules nt (Production (s,rules))
      | nt == s   = Just rules
      | otherwise = Nothing


instance IsDefinedBy a => IsDefinedBy (NonEmpty a) where

    alreadyDefinedBy nt = any (alreadyDefinedBy nt)


instance IsQueryable a => IsQueryable (NonEmpty a) where

    queryRules nt = asum . fmap (queryRules nt) 


instance IsDefinedBy a => IsDefinedBy [a] where

    alreadyDefinedBy nt = any (alreadyDefinedBy nt)


instance IsQueryable a => IsQueryable [a] where

    queryRules nt = asum . fmap (queryRules nt)


instance IsDefinedBy a => IsDefinedBy (Set a) where

    alreadyDefinedBy nt = any (alreadyDefinedBy nt)


instance (IsQueryable a, Ord a) => IsQueryable (Set a) where

    queryRules nt = asum . fmap (queryRules nt) . toList


instance (IsDefinedBy a, IsDefinedBy b) => IsDefinedBy (a,b) where

    alreadyDefinedBy nt (a,b) = nt `alreadyDefinedBy` a || nt `alreadyDefinedBy` b


instance (IsQueryable a, IsQueryable b) => IsQueryable (a,b) where

    queryRules nt (a,b) = asum $ queryRules nt a :| [ queryRules nt b ]


newtype G4Production = G4P (Text, Set G4Rule)


newtype G4Rule = G4R Text
    deriving newtype (Eq, Ord)


instance ToG4 Grammar where

    toG4 _title = foldMap (renderG4 . productionToG4) . toList
      where
        -- addTitle x = "grammar " <> title <> ";\n\n" <> x
        productionToG4 :: Production -> G4Production
        productionToG4 v | trace ("G4 -> " <> show (nonTerminal v)) False = undefined
        productionToG4 v =
          let toG4Key = coerce
              toG4Val = Set.map toG4Rule
          in  G4P . bimap toG4Key toG4Val . extractProduction $ v        

        toG4Rule = G4R . intercalate " " . go . toList
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


        renderG4 (G4P (sym, rules)) =
          let (rule1, others) = Set.deleteFindMin rules
--              indent = ("    " <>)
              pipe   = (" | " <>)
              colon  = (": " <>)
              text1  = (sym<>) . colon $ coerce rule1
              texts  = pipe  . coerce <$> toList others :: [Text]
          in  if null rules
              then ""
              else (<>"\n\n") . intercalate " " $ text1:texts


instance ToJSON Grammar where

    toJSON = object . fmap buildProduction . toList
      where
        buildProduction :: Production -> (Key, Value)
        buildProduction v | trace ("JSON -> " <> show (nonTerminal v)) False = undefined
        buildProduction v =
          let toObjKey = fromString . unpack . coerce
              toObjVal = toJSONList . toList
          in  bimap toObjKey toObjVal . extractProduction $ v


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

    show (Symbol (Left  nonTerm)) = unpack $ coerce nonTerm
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


instance Semigroup Rule where

    (<>) (Rule (t,as)) (Rule (b,bs)) = Rule (t, as <> [tok b] <> bs)


instance HasSuffixSymbol a => HasNonTerminal (NonEmpty a) where

    nonTerminal (x:|_) = NonTerminal "ONEORMORE" `appendSuffix` x


instance HasSuffixSymbol a  => HasNonTerminal (Padded a) where

    nonTerminal (Padded a) = NonTerminal "PADDED" `appendSuffix` a


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasNonTerminal (SepBy a b) where

    nonTerminal (a `SepBy` b) = (NonTerminal "SEPBY" `appendSuffix` a) `appendSuffix` b


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasNonTerminal (a :<>: b) where

    nonTerminal (a :<>: b) = (NonTerminal "PAIRED" `appendSuffix` a) `appendSuffix` b


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasNonTerminal (a :||: b) where

    nonTerminal (a :||: b) = (NonTerminal "EITHER" `appendSuffix` a) `appendSuffix` b


instance HasNonTerminal NonTerminal where

    nonTerminal = id


instance HasNonTerminal Production where

    nonTerminal (Production (x,_)) = x


instance {-# OVERLAPPABLE #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (NonEmpty a) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g ne@(a:|_) =
      let (rulesA, depsA) = getRulesAndDeps g a
          latter = Set.map (appendSymbols [note ne]) rulesA
      in  (Production (nonTerminal ne, rulesA <> latter), depsA)


instance {-# OVERLAPPING #-} HasProductions (NonEmpty Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule _ ne@(x:|_) = fromRulesAndNoDeps (nonTerminal ne)
            [ [ term x ]
            , [ term x, note ne ]
            ]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (Padded a) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g p@(Padded a) =
      let (rulesA, depsA) = getRulesAndDeps g a
          latter       = Set.map (<> [ " ", note spaces ]) rulesA
          spaces       = [" "] :: NonEmpty Terminal
          rulesNew     = fold $
              rulesA :|
              [ [[ " ", note spaces, note a ]]
              , latter
              , [[ " ", note spaces, note a, note spaces ]]
              ]
      in  (depsA <>) <$> fromRulesWithDeps g (nonTerminal p) rulesNew [spaces]


instance {-# OVERLAPPING #-} HasProductions (Padded Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g p@(Padded t) =
      let space  = " " :: Terminal
          spaces = [space] :: NonEmpty Terminal
      in  fromRulesWithDeps g (nonTerminal p)
            [ [                          term t ]
            , [ term space, note spaces, term t ]
            , [                          term t, note spaces ]
            , [ term space, note spaces, term t, note spaces ]
            ]
            [spaces]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasProductions b, HasSuffixSymbol a, HasSuffixSymbol b) => HasProductions (SepBy a b) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g s@(a `SepBy` b) =
      let (rulesA, depsA) = getRulesAndDeps g a
          latter = Set.map (appendSymbols [note b, note s ]) rulesA
      in  (depsA <>) <$> fromRulesWithDeps g (nonTerminal s) (rulesA <> latter) [b]


instance {-# OVERLAPPING #-} (HasProductions b, HasSuffixSymbol b) => HasProductions (SepBy Terminal b) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g s@(t `SepBy` b) = fromRulesWithDeps g (nonTerminal s)
            [ [ term t ]
            , [ term t, note b, note s]
            ]
            [ b ]


instance {-# OVERLAPPING #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (SepBy a Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g s@(a `SepBy` t) =
      let (rulesA, depsA) = getRulesAndDeps g a
          tThenA = Set.map (\r -> flip appendSymbols ([term t] <> r) [note s]) rulesA
      in  (Production (nonTerminal s, rulesA <> tThenA), depsA)


instance {-# OVERLAPPING #-} HasProductions (SepBy Terminal Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule _ s@(t1 `SepBy` t2) =
      fromRulesAndNoDeps (nonTerminal s)
            [ [ term t1  ]
            , [ term t1, term t2, note s ]
            ]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasProductions b, HasSuffixSymbol a, HasSuffixSymbol b) => HasProductions (a :<>: b) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g j@(a :<>: b) =
      let (rulesA, depsA) = getRulesAndDeps g a
          latter = Set.map (appendSymbols [note b]) rulesA
      in  (depsA <>) <$> fromRulesWithDeps g (nonTerminal j) (rulesA <> latter) [b]


instance {-# OVERLAPPING #-} (HasProductions b, HasSuffixSymbol b) => HasProductions (Terminal :<>: b) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g j@(t :<>: b) = fromRulesWithDeps g (nonTerminal j)
        [ [ term t, note b ]
        ]
        [ b ]


instance {-# OVERLAPPING #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (a :<>: Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g j@(a :<>: t) =
      let (rulesA, depsA) = getRulesAndDeps g a
      in  (Production (nonTerminal j, Set.map (<> [term t]) rulesA), depsA)


instance {-# OVERLAPPING #-} HasProductions (Terminal :<>: Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule _ j@(t1 :<>: t2) = fromRulesAndNoDeps (nonTerminal j)
        [ [ term t1, term t2 ]
        ]


instance {-# OVERLAPPABLE #-} (HasProductions a, HasProductions b, HasSuffixSymbol a, HasSuffixSymbol b) => HasProductions (a :||: b) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g e@(a :||: b) =
      let (rulesA, depsA) = getRulesAndDeps g a
          (rulesB, depsB) = getRulesAndDeps g b
      in  (Production (nonTerminal e, rulesA <> rulesB), depsA <> depsB)


instance {-# OVERLAPPING #-} (HasProductions b, HasSuffixSymbol b) => HasProductions (Terminal :||: b) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g e@(t :||: b) =
      let (rulesB, depsB) = getRulesAndDeps g b
      in  (Production (nonTerminal e, [[ term t ]] <> rulesB), depsB)


instance {-# OVERLAPPING #-} (HasProductions a, HasSuffixSymbol a) => HasProductions (a :||: Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule g e@(a :||: t) =
      let (rulesA, depsA) = getRulesAndDeps g a
      in  (Production (nonTerminal e, rulesA <> [[ term t ]]), depsA)


instance {-# OVERLAPPING #-} HasProductions (Terminal :||: Terminal) where

    productionRule _ x | trace (show (suffix x)) False = undefined
    productionRule _ e@(t1 :||: t2) = fromRulesAndNoDeps (nonTerminal e)
        [ [ term t1 ]
        , [ term t2 ]
        ]


instance HasProductions Production where

    productionRule _ x = (x, mempty)


instance HasSuffixSymbol a => HasSuffixSymbol (NonEmpty a) where

    suffix = nonTerminal


instance HasSuffixSymbol a => HasSuffixSymbol (Padded a) where

    suffix = nonTerminal


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasSuffixSymbol (SepBy a b) where

    suffix = nonTerminal


instance (HasSuffixSymbol a, HasSuffixSymbol b)  => HasSuffixSymbol (a :<>: b) where

    suffix = nonTerminal


instance {-# OVERLAPPABLE #-} (HasSuffixSymbol a, HasSuffixSymbol b)  => HasSuffixSymbol (a :||: b) where

    suffix = nonTerminal


instance {-# OVERLAPPING #-} HasSuffixSymbol (Terminal :||: Terminal) where

    suffix = nonTerminal 


instance HasSuffixSymbol Terminal where

    suffix (Terminal e) =
      let suff = case e of
                   Left  c   -> fromString $ "CHAR" <> escapeChar c
                   Right txt -> "TERM" <> txt
      in  NonTerminal suff


instance HasSuffixSymbol NonTerminal where

    suffix = id


instance HasTerminal Terminal where

    terminal = id


escapeTerm :: IsString s => String -> s
escapeTerm = fromString . foldMap escapeChar


escapeChar :: Char -> String
escapeChar = fmap (toUpper . advance) . flip showHex "" . fromEnum
  where
    advance =
      \case
        '0' -> 'A'
        '1' -> 'B'
        '2' -> 'C'
        '3' -> 'D'
        '4' -> 'E'
        '5' -> 'F'
        '6' -> 'G'
        '7' -> 'H'
        '8' -> 'I'
        '9' -> 'J'
        'A' -> 'K'
        'B' -> 'L'
        'C' -> 'M'
        'D' -> 'N'
        'E' -> 'O'
        _   -> 'P'


instance IsList Grammar where

    type (Item Grammar) = Production

    fromList = Grammar . fromList . fmap extractProduction

    toList (Grammar x) = Production <$> toList x


instance IsList Rule where

    type (Item Rule) = Symbol

    fromList =
      \case
        [] -> error "Cannot construct Rule from empty list []"
        Symbol (Right t):xs -> Rule (t, fromList xs)
        xs  -> error $ "Cannot construct Rule from list beginning with a NonTerminal:\n" <> (unwords $ show <$> xs)

    toList (Rule (t,x)) = Symbol (Right t) : toList x


appendSymbols :: Seq Symbol -> Rule -> Rule
appendSymbols sym (Rule (t,x)) = Rule (t, x <> sym)


appendSymbolsToSet :: Seq Symbol -> Set Rule -> Set Rule
appendSymbolsToSet xs = Set.map (appendSymbols xs)


instance IsString NonTerminal where

    fromString = NonTerminal . escapeTerm


instance IsString Terminal where

    fromString  [] = Terminal $ Right ""
    fromString  xs = Terminal . Right $ escapeTerm xs


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

    (<>) (NonTerminal x) (NonTerminal y) = NonTerminal $ x <> "VVV" <> y


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


{-# Specialise ruleWithDeps :: IsQueryable g => g -> Rule -> NonEmpty (NonEmpty Terminal) -> (Rule, Dependencies) #-}
ruleWithDeps :: (HasProductions a, IsQueryable g) => g -> Rule -> NonEmpty a -> (Rule, Dependencies)
ruleWithDeps g r = (,) <$> const r <*> dependencyN g


ruleWithDep2
  :: ( HasProductions a
     , HasProductions b
     , IsQueryable g
     )
  => g -> Rule -> (a, b) -> (Rule, Dependencies)
ruleWithDep2 g r = (,) <$> const r <*> dependency2 g


ruleWithDep3
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , IsQueryable g
     )
  => g -> Rule -> (a, b, c) -> (Rule, Dependencies)
ruleWithDep3 g r = (,) <$> const r <*> dependency3 g


ruleWithDep4
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , HasProductions d
     , IsQueryable g
     )
  => g -> Rule -> (a, b, c, d) -> (Rule, Dependencies)
ruleWithDep4 g r = (,) <$> const r <*> dependency4 g


fromRulesAndNoDeps :: NonTerminal -> Set Rule -> (Production, Dependencies)
fromRulesAndNoDeps t rs = (Production (t, rs), mempty)


fromRulesWithDeps :: (HasProductions a, IsQueryable b) => b -> NonTerminal -> Set Rule -> NonEmpty a -> (Production, Dependencies)
fromRulesWithDeps g t rs = (,) <$> const (Production (t, rs)) <*> dependencyN g


fromRulesWithDeps2
  :: ( HasProductions a
     , HasProductions b
     , IsQueryable g
     )
  => g -> NonTerminal -> Set Rule -> (a, b) -> (Production, Dependencies)
fromRulesWithDeps2 g t rs = (,) <$> const (Production (t, rs)) <*> dependency2 g


fromRulesWithDeps3
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , IsQueryable g
     )
  => g -> NonTerminal -> Set Rule -> (a, b, c) -> (Production, Dependencies)
fromRulesWithDeps3 g t rs = (,) <$> const (Production (t, rs)) <*> dependency3 g


fromRulesWithDeps4
  :: ( HasProductions a
     , HasProductions b
     , HasProductions c
     , HasProductions d
     , IsQueryable g
     )
  => g -> NonTerminal -> Set Rule -> (a, b, c, d) -> (Production, Dependencies)
fromRulesWithDeps4 g t rs = (,) <$> const (Production (t, rs)) <*> dependency4 g


dependency2
  :: ( HasProductions a
     , HasProductions b
     , IsQueryable g
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
     , IsQueryable g
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
     , IsQueryable g
     )
  => g
  -> (a, b, c, d)
  -> Dependencies
dependency4 g (a, b, c, d) = applyProduction g d $ dependency3 g (a, b, c)


dependencyN :: (HasProductions a, IsQueryable g) => g -> NonEmpty a -> Dependencies
dependencyN x = foldr (applyProduction x) mempty . toList


applyProduction :: (IsQueryable a, HasProductions b) => a -> b -> Dependencies -> Dependencies
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
     , IsQueryable g
     )
  => g
  -> NonTerminal
  -> a
  -> (Production, Dependencies)
enumerableProductions g label val =
    let ref = Production (label, rules)
        (rules, deps) = bimap fromList fold . unzip . fmap (ruleOfValue (g,ref)) $ enumerableValuesOf val
    in  (ref, deps)


enumerableGrammar :: HasProductions a => a -> GrammarBuilder
enumerableGrammar = GB . gBuild . (:|[])


gBuild :: (Foldable f, HasProductions a) => f a -> Grammar -> Grammar
gBuild xs grammar =
    let (deps, grammar') = foldl' define (mempty, grammar) xs
    in  case grammar' `definitionsRemovedFrom` deps of
          Deps Nothing    -> grammar'
          Deps (Just set) -> gBuild set grammar'


define :: HasProductions a => (Dependencies, Grammar) -> a -> (Dependencies, Grammar)
define acc@(more, g@(Grammar m)) v
      | trace ("DEFINE $ " <> show (nonTerminal v) <> " >< " <> show (nonTerminal v `alreadyDefinedBy` g)) False = undefined
      | nonTerminal v `alreadyDefinedBy` g = acc
      | otherwise =
         let (new, deps) = review g v
         in  (more <> deps, Grammar $ m `append` new)


review :: (HasProductions a, IsQueryable b) => b -> a -> ((NonTerminal, Set Rule), Dependencies)
review g = bimap extractProduction id . productionRule g


append :: Map NonTerminal b -> (NonTerminal, b) -> Map NonTerminal b
append _ (x,_) | trace ("Adding to grammar: " <> show x) False = undefined
append m v = uncurry insert v m
--append = flip $ uncurry insert


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


mkNonTerminal :: IsString s => [Char] -> b -> s
mkNonTerminal = const . fromString . fmap toUpper . foldMap f
  where
    f c | not $ isAlpha c = escapeChar c
        | otherwise       = [c]


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


getRulesAndDeps :: (HasProductions a, IsQueryable g) => g -> a -> (Set Rule, Dependencies)
getRulesAndDeps g a =
    let sym = nonTerminal a
    in  case sym `queryRules` g of
          Just rules -> (rules, mempty)
          Nothing    ->
            let ref = Production (sym, mempty)
            in  bimap (snd . extractProduction) (removeDependency sym) $ productionRule (g, ref) a


removeDependency :: HasNonTerminal a => a -> Dependencies -> Dependencies
removeDependency = const id
{-
removeDependency _ d@(Deps  Nothing  ) = d
removeDependency e   (Deps (Just set)) =
   let sym = nonTerminal e
   in  Deps . Just $ filter ((sym==) . nonTerminal) set
-}
