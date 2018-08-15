{-# LANGUAGE StrictData #-}

module Servant.Flow.FlowType where

import           Data.Functor.Foldable
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics


class FlowTyped a where
    flowType :: Proxy a -> FlowType

    default
        flowType :: (Generic a, GFlowTyped (Rep a)) => Proxy a -> FlowType
    flowType _ = gFlowType (from (undefined :: a))

-- Primative instances
instance FlowTyped Int where
    flowType _ = Fix $ Prim Number

instance FlowTyped Bool where
    flowType _ = Fix $ Prim Boolean

instance FlowTyped Text where
    flowType _ = Fix $ Prim String

instance FlowTyped a => FlowTyped (Maybe a) where
    flowType _ = Fix . Nullable $ flowType (Proxy @a)

-- Generic instances
class GFlowTyped f where
    gFlowType :: f x -> FlowType

-- Single-constructor multi-field records
instance (GFlowRecordFields f, GFlowRecordFields g)
    => GFlowTyped (D1 m1 (C1 m2 (f :*: g))) where
        gFlowType _ = Fix . ExactObject $ recordFields (undefined :: (f :*: g) ())

-- Simple sum types
instance GSimpleSum (f :+: g) => GFlowTyped (D1 m (f :+: g)) where
    gFlowType _ = Fix . Sum . fmap (Fix . Literal . LitString) $
        simpleSumOptions (undefined :: (f :+: g) ())

-- Use an instance that already exists
instance FlowTyped a => GFlowTyped (K1 i a) where
    gFlowType _ = flowType (Proxy @a)

-- Record product type helper class
class GFlowRecordFields f where
    recordFields :: f x -> [(Text, FlowType)]

instance (FlowTyped a, Selector s) => GFlowRecordFields (S1 s (K1 R a)) where
    recordFields _ =
        [(fromString $ selName (undefined :: S1 s (K1 R a) ()) , flowType (Proxy @a))]

instance (GFlowRecordFields f, GFlowRecordFields g) => GFlowRecordFields (f :*: g) where
    recordFields _ = recordFields (undefined :: f ()) <> recordFields (undefined :: g ())


-- Simple sum type helper class
class GSimpleSum f where
    simpleSumOptions :: f x -> [Text]

instance Constructor meta => GSimpleSum (M1 C meta U1) where
    simpleSumOptions _ = pure . fromString $ conName (undefined :: C1 meta U1 ())

instance (GSimpleSum f, GSimpleSum g) => GSimpleSum (f :+: g) where
    simpleSumOptions _ =
        simpleSumOptions (undefined :: f ()) <> simpleSumOptions (undefined :: g ())


data PrimType
  = Boolean
  | Number
  | String
  | Any
  deriving (Show, Eq, Ord)


type FlowType = Fix FlowTypeF

data FlowTypeF a
    = Prim PrimType
    | ExactObject [(Text, a)]
    | Array a
    | Nullable a
    | Sum [a]   -- "Union" in Flow terminology
    | Literal Lit
    deriving (Show, Eq, Functor, Traversable, Foldable)

data Lit
    = LitString Text
    deriving (Show, Eq)

showLiteral :: Lit -> Text
showLiteral (LitString txt) = fromString $ show txt

primBoolean, primNumber, primString, primAny :: FlowType
primBoolean = Fix $ Prim Boolean
primNumber  = Fix $ Prim Number
primString  = Fix $ Prim String
primAny     = Fix $ Prim Any


showFlowTypeInComment :: FlowType -> Text
showFlowTypeInComment t = "/* : " <> showFlowType t <> " */"


inBrackets :: Text -> Text
inBrackets t = "{ " <> t <> " }"

inSuperBrackets :: Text -> Text
inSuperBrackets t = "{| " <> t <> " |}"

showFlowType :: FlowType -> Text
showFlowType = cata showFlowTypeF

showFlowTypeF :: FlowTypeF Text -> Text
showFlowTypeF (Prim Boolean)  = "boolean"
showFlowTypeF (Prim Number)   = "number"
showFlowTypeF (Prim String)   = "string"
showFlowTypeF (Prim Any)      = "any"
showFlowTypeF (Nullable t)    = "?" <> t
showFlowTypeF (Array a)       = a <> "[]"
showFlowTypeF (Sum l)         = T.intercalate " | " l
showFlowTypeF (Literal lit)   = showLiteral lit
showFlowTypeF (ExactObject l) = inSuperBrackets $
    T.intercalate ", " (fmap (\(n, t) -> n <> " : " <> t) l)
