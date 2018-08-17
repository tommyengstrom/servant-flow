{-# LANGUAGE StrictData #-}

module Servant.Flow.Internal where

import           Data.Aeson            (Options (..), defaultOptions)
import           Data.Bifunctor        (first)
import           Data.Foldable         (toList)
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


class FlowTyped a where
    flowTypeInfo :: Proxy a -> FlowTypeInfo

    default
        flowTypeInfo :: (Generic a, GFlowTyped (Rep a)) => Proxy a -> FlowTypeInfo
    flowTypeInfo pa = genericFlowType defaultOptions pa

    flowType :: Proxy a -> FlowType
    flowType = forgetNames . flowTypeInfo


genericFlowType :: forall a. (Generic a, GFlowTyped (Rep a))
                => Options -> Proxy a -> FlowTypeInfo
genericFlowType opts _ = nameless $ gFlowType opts (from (undefined :: a))


-- Primative instances
instance FlowTyped Int where
    flowTypeInfo _ = nameless primNumber

instance FlowTyped Int64 where
    flowTypeInfo _ = nameless primNumber

instance FlowTyped Float where
    flowTypeInfo _ = nameless primNumber

instance FlowTyped Double where
    flowTypeInfo _ = nameless primNumber

instance FlowTyped Bool where
    flowTypeInfo _ = nameless primNumber

instance FlowTyped Text where
    flowTypeInfo _ = nameless primString

instance FlowTyped UTCTime where
    flowTypeInfo _ = nameless primString

instance FlowTyped Day where
    flowTypeInfo _ = nameless primString

instance FlowTyped LocalTime where
    flowTypeInfo _ = nameless primString

instance FlowTyped NoContent where
    flowTypeInfo _ = nameless primVoid


instance FlowTyped a => FlowTyped (Maybe a) where
    flowTypeInfo _ = Fix . L1 . Nullable $ flowTypeInfo (Proxy @a)

instance FlowTyped a => FlowTyped [a] where
    flowTypeInfo _ = Fix . L1 . Array $ flowTypeInfo (Proxy @a)

instance (Ord a, FlowTyped a) => FlowTyped (Set a) where
    flowTypeInfo _ = Fix . L1 . Array $ flowTypeInfo (Proxy @a)

instance (Ord k, FlowObjectKey k, FlowTyped a) => FlowTyped (Map k a) where
    flowTypeInfo _ = Fix . L1 . Object $
        [IndexerProperty (nameless primString) $ flowTypeInfo (Proxy @a)]


-- | Arbitrary types cannot be object keys but need some reasonable textual representation
--   "Data.Aeson.ToJSONKey" has more complex functionality than we support.
class FlowObjectKey a

instance FlowObjectKey String
instance FlowObjectKey Text


-- Generic instances
class GFlowTyped f where
    gFlowType :: Options -> f x -> FlowType

-- Single-constructor records
instance GFlowRecordFields f => GFlowTyped (D1 m1 (C1 m2 f)) where
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
    gFlowType _ _ = flowType (Proxy @a)

-- Record product type helper class
class GFlowRecordFields f where
    recordFields :: f x -> [(String, FlowType)]

instance (FlowTyped a, Selector s) => GFlowRecordFields (S1 s (K1 R a)) where
    recordFields _ =
        [(selName (undefined :: S1 s (K1 R a) ()) , flowType (Proxy @a))]

instance (GFlowRecordFields f, GFlowRecordFields g) => GFlowRecordFields (f :*: g) where
    recordFields _ = recordFields (undefined :: f ()) <> recordFields (undefined :: g ())


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
    deriving (Show, Eq, Functor, Traversable, Foldable)

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
renderFlowTypeF (ExactObject l) = inSuperBrackets . T.intercalate ", " $
    (\(n, t) -> n <> " : " <> t) <$> l


-- | Annotate a place in a type expression as correspdonding to a named definition.
--   No assumptions are made about the name, such as any relationship to the haskell one.
data Named a = Named
    { namedName :: Text
    , namedBody :: a
    } deriving (Functor, Foldable)

type FlowTypeInfoF = FlowTypeF :+: Named
type FlowTypeInfo  = Fix FlowTypeInfoF

forgetNamesF :: (FlowTypeF :+: Named) FlowType -> FlowType
forgetNamesF (L1 ty) = Fix ty
forgetNamesF (R1 r)  = namedBody r

forgetNames :: FlowTypeInfo -> FlowType
forgetNames = cata forgetNamesF

nameless :: FlowType -> FlowTypeInfo
nameless = cata $ Fix . L1

named :: Text -> FlowTypeInfo -> FlowTypeInfo
named n = Fix . R1 . Named n

renderFlowTypeWithRefsF :: FlowTypeInfoF Text -> Text
renderFlowTypeWithRefsF (L1 ty) = renderFlowTypeF ty
renderFlowTypeWithRefsF (R1 n)  = namedName n

type Env = [(Text, FlowTypeInfo)]

getTypeEnvF :: FlowTypeInfoF Env -> Env
getTypeEnvF = concat . toList
