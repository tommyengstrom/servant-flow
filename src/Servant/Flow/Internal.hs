{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}
module Servant.Flow.Internal where

import           Data.Aeson            (Options (..), defaultOptions)
import           Data.Bifunctor        (first)
import           Data.Functor.Foldable
import           Data.Int              (Int64)
import           Data.Map              (Map)
import           Data.Monoid           ((<>))
import           Data.Proxy
import           Data.Set              (Set)
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (Day, LocalTime, UTCTime)
import           GHC.Generics
import           Servant.API           (NoContent)
import Data.Maybe

class FlowTyped a where
    flowType :: Proxy a -> FlowType

    default flowType :: (Generic a, GFlowTyped (Rep a)) => Proxy a -> FlowType
    flowType pa = genericFlowType defaultOptions pa

    -- | Returning `Just t` mean t will be used by types referencing it.
    --   Returning `Nothing` mean the type definition will be inserted directly.
    --   Not using references is a very bad idea for recursive types!
    flowTypeName :: Proxy a -> Maybe FlowType

    default flowTypeName :: (Generic a, GTypeName (Rep a)) => Proxy a -> Maybe FlowType
    flowTypeName = genericFlowTypeName


genericFlowType :: forall a. (Generic a, GFlowTyped (Rep a))
                => Options -> Proxy a -> FlowType
genericFlowType opts _ = gFlowType opts (from (undefined :: a))


genericFlowTypeName :: forall a. (Generic a, GTypeName (Rep a))
                    => Proxy a -> Maybe FlowType
genericFlowTypeName _ = Just . Fix . NamedType $ gtypename (from (undefined :: a))

class GTypeName f where
  gtypename :: f a -> Text

instance (Datatype c) => GTypeName (M1 i c f) where
  gtypename m = T.pack $ datatypeName m

-- Primative instances
instance FlowTyped Int where
    flowType _ = Fix $ Prim Number
    flowTypeName _ = Nothing

instance FlowTyped Int64 where
    flowType _ = Fix $ Prim Number
    flowTypeName _ = Nothing

instance FlowTyped Float where
    flowType _ = Fix $ Prim Number
    flowTypeName _ = Nothing

instance FlowTyped Double where
    flowType _ = Fix $ Prim Number
    flowTypeName _ = Nothing

instance FlowTyped Bool where
    flowType _ = Fix $ Prim Boolean
    flowTypeName _ = Nothing

instance FlowTyped Text where
    flowType _ = Fix $ Prim String
    flowTypeName _ = Nothing

instance FlowTyped String where
    flowType _ = Fix $ Prim String
    flowTypeName _ = Nothing

instance FlowTyped UTCTime where
    flowType _ = Fix $ Prim String
    flowTypeName _ = Nothing

instance FlowTyped Day where
    flowType _ = Fix $ Prim String
    flowTypeName _ = Nothing

instance FlowTyped LocalTime where
    flowType _ = Fix $ Prim String
    flowTypeName _ = Nothing

instance FlowTyped NoContent where
    flowType _ = Fix $ Prim Void
    flowTypeName _ = Nothing


instance FlowTyped a => FlowTyped (Maybe a) where
    flowType _ = Fix . Nullable $ flowTypePreferReference (Proxy @a)
    flowTypeName _ = Nothing

instance FlowTyped a => FlowTyped [a] where
    flowType _ = Fix . Array $ flowTypePreferReference (Proxy @a)
    flowTypeName _ = Nothing

instance (Ord a, FlowTyped a) => FlowTyped (Set a) where
    flowType _ = Fix . Array $ flowTypePreferReference (Proxy @a)
    flowTypeName _ = Nothing

instance (Ord k, FlowObjectKey k, FlowTyped a) => FlowTyped (Map k a) where
    flowType _ = Fix . Object $ [IndexerProperty (Fix $ Prim String)
               $ flowTypePreferReference (Proxy @a)]
    flowTypeName _ = Nothing


-- | Arbitrary types cannot be object keys but need some reasonable textual representation
--   "Data.Aeson.ToJSONKey" has more complex functionality than we support.
class FlowObjectKey a

instance FlowObjectKey String
instance FlowObjectKey Text


-- Generic instances
class GFlowTyped f where
    gFlowType :: Options -> f x -> FlowType

-- Single-constructor records
instance (Datatype m1, GFlowRecordFields f) => GFlowTyped (D1 m1 (C1 m2 f)) where
    gFlowType opts _
        = Fix . ExactObject
        . fmap (first $ fromString . (fieldLabelModifier opts))
        $ recordFields (undefined :: f ())

-- Simple sum types
instance GSimpleSum (f :+: g) => GFlowTyped (D1 m (f :+: g)) where
    gFlowType opts _ = Fix . Sum . fmap (Fix . Literal . LitString) $
        simpleSumOptions opts (undefined :: (f :+: g) ())

-- Use an instance that already exists
instance FlowTyped a => GFlowTyped (K1 i a) where
    gFlowType _ _ = flowTypePreferReference (Proxy @a)

-- Record product type helper class
class GFlowRecordFields f where
    recordFields :: f x -> [(String, FlowType)]

instance (FlowTyped a, Selector s) => GFlowRecordFields (S1 s (K1 R a)) where
    recordFields _ =
        [(selName (undefined :: S1 s (K1 R a) ()) , flowTypePreferReference (Proxy @a))]

instance (GFlowRecordFields f, GFlowRecordFields g) => GFlowRecordFields (f :*: g) where
    recordFields _ = recordFields (undefined :: f ()) <> recordFields (undefined :: g ())

flowTypePreferReference :: FlowTyped a => Proxy a -> FlowType
flowTypePreferReference p = fromMaybe (flowType p) $ flowTypeName p

-- Simple sum type helper class
class GSimpleSum f where
    simpleSumOptions :: Options -> f x -> [Text]

instance Constructor meta => GSimpleSum (M1 C meta U1) where
    simpleSumOptions opts _
        = pure
        . fromString
        . constructorTagModifier opts
        $ conName (undefined :: C1 meta U1 ())

instance (GSimpleSum f, GSimpleSum g) => GSimpleSum (f :+: g) where
    simpleSumOptions opts _ =
        simpleSumOptions opts (undefined :: f ()) <>
        simpleSumOptions opts (undefined :: g ())


data PrimType
  = Boolean
  | Number
  | String
  | Any
  | AnyObject
  | Void
  deriving (Show, Eq, Ord)


type FlowType = Fix FlowTypeF

data FlowTypeF a
    = Prim PrimType
    | ExactObject [(Text, a)]
    | Array a
    | Nullable a
    | Sum [a]   -- "Union" in Flow terminology
    | Literal Lit
    | Object [PropertyF a]
    | NamedType Text
    | Promise a
    deriving (Functor, Traversable, Foldable)

data DeclaredType = DeclaredType
    { getTypename :: Text
    }

data Lit
    = LitString Text
    deriving (Show, Eq)

data PropertyF a
    -- | A regular object field
    = Property Text a
    -- | A 'Map'-like field with a specific key type.
    | IndexerProperty a a -- like a 'Map'
    deriving (Show, Eq, Functor, Traversable, Foldable)
    -- NamedIndexerProperty Text a a  -- Carries a simple comment-like annotation

showLiteral :: Lit -> Text
showLiteral (LitString txt) = fromString $ show txt

-- Primative FlowTypes for export
primBoolean, primNumber, primString, primAny, primAnyObject, primVoid :: FlowType
primBoolean   = Fix $ Prim Boolean
primNumber    = Fix $ Prim Number
primString    = Fix $ Prim String
primAny       = Fix $ Prim Any
primAnyObject = Fix $ Prim AnyObject
primVoid      = Fix $ Prim Void


renderFlowTypeInComment :: FlowType -> Text
renderFlowTypeInComment t = "/* : " <> renderFlowType t <> " */"

inParens :: Text -> Text
inParens t = "(" <> t <> ")"

inBrackets :: Text -> Text
inBrackets t = "{ " <> t <> " }"

inSuperBrackets :: Text -> Text
inSuperBrackets t = "{| " <> t <> " |}"

renderFlowType :: FlowType -> Text
renderFlowType = cata renderFlowTypeF

renderPrimative :: PrimType -> Text
renderPrimative Boolean   = "boolean"
renderPrimative Number    = "number"
renderPrimative String    = "string"
renderPrimative Any       = "any"
renderPrimative AnyObject = "{}" -- unclear if/how this differs from "Object"
renderPrimative Void      = "void"

renderProperty :: PropertyF Text -> Text
renderProperty (Property fieldName ty)    = fieldName <> ": " <> ty
renderProperty (IndexerProperty keyTy ty) = "[" <> keyTy <> "]: " <> ty

renderFlowTypeF :: FlowTypeF Text -> Text
renderFlowTypeF (Prim prim)     = renderPrimative prim
renderFlowTypeF (Nullable t)    = "?" <> inParens t
renderFlowTypeF (Array a)       = inParens a <> "[]"
renderFlowTypeF (Sum l)         = T.intercalate " | " l
renderFlowTypeF (Literal lit)   = showLiteral lit
renderFlowTypeF (Object ps)     = inBrackets . T.intercalate ", " $ renderProperty <$> ps
renderFlowTypeF (ExactObject l) = inSuperBrackets
                                . T.intercalate ", "
                                $ (\(n, t) -> n <> " : " <> t) <$> l
renderFlowTypeF (Promise t)     = "Promise<" <> t <> ">"
renderFlowTypeF (NamedType t)   = t



--renderTypeDef :: DeclaredType -> Text
--renderTypeDef (DeclaredType n p) = "type " <> n <> " = " <> renderFlowType (flowType p)
