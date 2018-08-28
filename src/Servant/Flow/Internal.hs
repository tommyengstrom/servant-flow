{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Flow.Internal where

import           Data.Aeson     (Options (..), defaultOptions)
import           Data.Bifunctor (first)
import           Data.Foldable  (toList)
import           Data.Int       (Int64)
import           Data.Map       (Map)
import           Data.Monoid    ((<>))
import           Data.Proxy
import           Data.Set       (Set)
import           Data.String
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Time      (Day, LocalTime, UTCTime)
import           GHC.Generics
import           Recursed
import           Servant.API    (NoContent)


data FlowType
    = Prim PrimType
    | ExactObject [(Text, FlowType)]
    | Array FlowType
    | Nullable FlowType
    -- | Called "Union" in Flow terminology
    | Sum [FlowType]
    | Literal Lit
    | Object [PropertyF FlowType]
    deriving (Show, Eq)

data PrimType
    = Boolean
    | Number
    | String
    | Any
    | AnyObject
    | Void
    deriving (Show, Eq, Ord)

data Lit
    = LitString Text
    deriving (Show, Eq)

data PropertyF a
    -- | A regular object field
    = Property Text a
    -- | A 'Map'-like field with a specific key type.
    | IndexerProperty a a
    deriving (Show, Eq, Functor, Traversable, Foldable)
    -- NamedIndexerProperty Text a a  -- Carries a simple comment-like annotation

$(recursed ''FlowType)


-- | Annotate a place in a type expression as correspdonding to a named definition.
--   No assumptions are made about the name, such as any relationship to the haskell one.
data Named a = Named
    { namedName :: Text
    , namedBody :: a
    } deriving (Functor, Foldable)

type FlowTypeInfoF = FlowTypeF :+: Named
type FlowTypeInfo  = Fix FlowTypeInfoF


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
    flowTypeInfo _ = nameless $ Prim Number

instance FlowTyped Int64 where
    flowTypeInfo _ = nameless $ Prim Number

instance FlowTyped Float where
    flowTypeInfo _ = nameless $ Prim Number

instance FlowTyped Double where
    flowTypeInfo _ = nameless $ Prim Number

instance FlowTyped Bool where
    flowTypeInfo _ = nameless $ Prim Number

instance FlowTyped Text where
    flowTypeInfo _ = nameless $ Prim String

instance FlowTyped UTCTime where
    flowTypeInfo _ = nameless $ Prim String

instance FlowTyped Day where
    flowTypeInfo _ = nameless $ Prim String

instance FlowTyped LocalTime where
    flowTypeInfo _ = nameless $ Prim String

instance FlowTyped NoContent where
    flowTypeInfo _ = nameless $ Prim Void


instance FlowTyped a => FlowTyped (Maybe a) where
    flowTypeInfo _ = Fix . L1 . Nullable_ $ flowTypeInfo (Proxy @a)

instance FlowTyped a => FlowTyped [a] where
    flowTypeInfo _ = Fix . L1 . Array_ $ flowTypeInfo (Proxy @a)

instance (Ord a, FlowTyped a) => FlowTyped (Set a) where
    flowTypeInfo _ = Fix . L1 . Array_ $ flowTypeInfo (Proxy @a)

instance (Ord k, FlowObjectKey k, FlowTyped a) => FlowTyped (Map k a) where
    flowTypeInfo _ = Fix . L1 . Object_ $
        [IndexerProperty (nameless $ Prim String) $ flowTypeInfo (Proxy @a)]


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
        = ExactObject
        . fmap (first $ fromString . (fieldLabelModifier opts))
        $ recordFields (undefined :: f ())

-- Simple sum types
instance GSimpleSum (f :+: g) => GFlowTyped (D1 m (f :+: g)) where
    gFlowType opts _ = Sum . fmap (Literal . LitString) $
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



showLiteral :: Lit -> Text
showLiteral (LitString txt) = fromString $ show txt

inParens :: Text -> Text
inParens t = "(" <> t <> ")"

inBrackets :: Text -> Text
inBrackets t = "{ " <> t <> " }"

inSuperBrackets :: Text -> Text
inSuperBrackets t = "{| " <> t <> " |}"


renderFlowType :: FlowType -> Text
renderFlowType = catamap renderFlowTypeF

renderFlowTypeF :: Algebra FlowTypeF Text
renderFlowTypeF (Prim_ prim)     = renderPrimative prim
renderFlowTypeF (Nullable_ t)    = "?" <> inParens t
renderFlowTypeF (Array_ a)       = inParens a <> "[]"
renderFlowTypeF (Sum_ l)         = T.intercalate " | " l
renderFlowTypeF (Literal_ lit)   = showLiteral lit
renderFlowTypeF (Object_ ps)     = inBrackets . T.intercalate ", " $ renderProperty <$> ps
renderFlowTypeF (ExactObject_ l) = inSuperBrackets . T.intercalate ", " $
    (\(n, t) -> n <> " : " <> t) <$> l

renderPrimative :: PrimType -> Text
renderPrimative Boolean   = "boolean"
renderPrimative Number    = "number"
renderPrimative String    = "string"
renderPrimative Any       = "any"
renderPrimative AnyObject = "{}" -- unclear if/how this differs from "Object"
renderPrimative Void      = "void"

renderProperty :: Algebra PropertyF Text
renderProperty (Property fieldName ty)    = fieldName <> ": " <> ty
renderProperty (IndexerProperty keyTy ty) = "[" <> keyTy <> "]: " <> ty

renderFlowTypeWithReferences :: FlowTypeRef -> Text
renderFlowTypeWithReferences = cata renderFlowTypeRefF

renderFlowTypeRefF :: Algebra (FlowTypeF :+: Ref) Text
renderFlowTypeRefF (L1 ty)      = renderFlowTypeF ty
renderFlowTypeRefF (R1 (Ref n)) = n


data Rendering = Flattened | Referenced

renderType :: Rendering -> FlowTypeInfo -> Text
renderType Flattened  = renderFlowType . forgetNames
renderType Referenced = renderFlowTypeWithReferences . toReferenced


forgetNamesF :: Algebra (FlowTypeF :+: Named) FlowType
forgetNamesF (L1 ty) = unfixAlg ty
forgetNamesF (R1 r)  = namedBody r

-- | Discard all type name information in the 'FlowTypeInfo' leaving just the 'FlowType'.
forgetNames :: FlowTypeInfo -> FlowType
forgetNames = cata forgetNamesF

-- | Convert a 'FlowType' to a 'FlowTypeInfo' without any type name information.
nameless :: FlowType -> FlowTypeInfo
nameless = catamap (Fix . L1)

-- | Annotate a 'FlowTypeInfo' expression as corresponding to a named type definition.
named :: Text -> FlowTypeInfo -> FlowTypeInfo
named n = Fix . R1 . Named n

-- | Create a 'FlowTypeInfo' by specifying the flow type name of the provided 'FlowType'.
withName :: Text -> FlowType -> FlowTypeInfo
withName n = named n . nameless


type Env = [(Text, FlowTypeInfo)]

getEnv :: FlowTypeInfo -> Env
getEnv = para getEnvR

getEnvR :: RAlgebra FlowTypeInfoF Env
getEnvR (L1 fpair)                   = snd =<< toList fpair
getEnvR (R1 (Named name (body,env))) = [(name, body)] <> env


type RefEnv = [(Text, FlowTypeRef)]

getRefEnv :: FlowTypeInfo -> RefEnv
getRefEnv = fmap (fmap toReferenced) . getEnv


-- | A Reference to a defined flow type. Differs from 'Named' in that it does not also
--   keep the value of the referenced type expression.
newtype Ref a = Ref Text deriving Functor

type FlowTypeRef = Fix (FlowTypeF :+: Ref)

-- | Drop any 'Named' subexpressions and instead just keep the 'Ref' to the type.
--   This function is mutually recursive with 'toReferencedAlg'.
toReferenced :: FlowTypeInfo -> FlowTypeRef
toReferenced = para toReferencedAlg

toReferencedAlg :: RAlgebra FlowTypeInfoF FlowTypeRef
toReferencedAlg (R1 n)     = Fix . R1 . Ref $ namedName n
toReferencedAlg (L1 infoF) = Fix $ toReferenced . fst <$> (L1 infoF)


primBoolean, primNumber, primString, primAny, primAnyObject, primVoid :: FlowTypeInfo
primBoolean   = nameless $ Prim Boolean
primNumber    = nameless $ Prim Number
primString    = nameless $ Prim String
primAny       = nameless $ Prim Any
primAnyObject = nameless $ Prim AnyObject
primVoid      = nameless $ Prim Void
