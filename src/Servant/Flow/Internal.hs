{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData         #-}

module Servant.Flow.Internal

    ( module Servant.Flow.Internal
    , Fix (..)

) where

import           Data.Aeson            (Options (..), defaultOptions)
import           Data.Bifunctor        (first)
import           Data.Foldable         (toList)
import           Data.Functor.Foldable
import           Data.Int              (Int64)
import           Data.List.NonEmpty    (NonEmpty (..))
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


type FlowType = Fix FlowTypeF

-- | Flow Types
data FlowTypeF a
    = Prim PrimType
    | ExactObject [(Text, a)]
    | Array a
    | Nullable a
    -- | Called "Union" in Flow terminology
    | Sum [a]
    | Literal Lit
    | Object [PropertyF a]
    deriving (Show, Functor, Foldable, Traversable)

-- | Primative types
data PrimType
    = Boolean
    | Number
    | String
    | Any
    | AnyObject
    | Void
    deriving (Show, Eq)

-- | Literal value types
data Lit
    = LitString Text
    deriving (Show, Eq)

-- | Object field types
data PropertyF a
    -- | A regular object field
    = Property Text a
    -- | A 'Map'-like field with a specific key type.
    | IndexerProperty a a
    deriving (Show, Eq, Functor, Traversable, Foldable)
    -- NamedIndexerProperty Text a a  -- Carries a simple comment-like annotation


primBoolean, primNumber, primString, primAny, primAnyObject, primVoid :: FlowTypeInfo
primBoolean   = nameless . Fix $ Prim Boolean
primNumber    = nameless . Fix $ Prim Number
primString    = nameless . Fix $ Prim String
primAny       = nameless . Fix $ Prim Any
primAnyObject = nameless . Fix $ Prim AnyObject
primVoid      = nameless . Fix $ Prim Void


------------------------------------------------------------------------------------------
--  Annotated Flow Types
------------------------------------------------------------------------------------------

-- | Annotate a place in a type expression as correspdonding to a named definition.
--   No assumptions are made about the name, such as any relationship to the haskell one.
data Named a = Named
    { namedName :: Text
    , namedBody :: a
    } deriving (Functor, Foldable, Show)

type FlowTypeInfoF = FlowTypeF :+: Named
type FlowTypeInfo  = Fix FlowTypeInfoF

-- | Annotate a 'FlowTypeInfo' expression as corresponding to a named type definition.
named :: Text -> FlowTypeInfo -> FlowTypeInfo
named n = Fix . R1 . Named n

-- | Convert a 'FlowType' to a 'FlowTypeInfo' without any type name information.
nameless :: FlowType -> FlowTypeInfo
nameless = cata (Fix . L1)

-- | Convert a 'FlowType' to a 'FlowTypeInfo' while providing a name.
withName :: Text -> FlowType -> FlowTypeInfo
withName n = named n . nameless

-- | Discard all type name information in the 'FlowTypeInfo' leaving just the 'FlowType'.
forgetNames :: FlowTypeInfo -> FlowType
forgetNames = cata forgetNamesF
    where
        forgetNamesF :: (FlowTypeF :+: Named) FlowType -> FlowType
        forgetNamesF (L1 ty) = Fix ty
        forgetNamesF (R1 r)  = namedBody r



-- | A Reference to a defined flow type. Differs from 'Named' in that it does not also
--   keep the value of the referenced type expression.
newtype Ref a = Ref Text deriving (Functor, Show)

type FlowTypeRef = Fix (FlowTypeF :+: Ref)

-- | Drop any 'Named' subexpressions and instead just keep the 'Ref' to the type.
toReferenced :: FlowTypeInfo -> FlowTypeRef
toReferenced = para toReferencedRAlg
    where
        toReferencedRAlg :: FlowTypeInfoF (Fix FlowTypeInfoF, FlowTypeRef) -> FlowTypeRef
        toReferencedRAlg (R1 n)     = Fix . R1 . Ref $ namedName n
        toReferencedRAlg (L1 infoF) = Fix $ toReferenced . fst <$> L1 infoF



-- | Get the list of all named types referenced in a given 'FlowTypeInfo'.
getEnv :: FlowTypeInfo -> [(Text, FlowTypeRef)]
getEnv = (fmap . fmap $ toReferenced) . go []
    where
        -- Some kind of variation on a 'para'.
        -- Needs the list to stop recusion on self-referencing flow types.
        go :: [Text] -> FlowTypeInfo -> [(Text, FlowTypeInfo)]
        go ns fx = case unfix fx of
            L1 l -> concat $ go ns <$> l
            R1 (Named n b)
                | n `elem` ns -> []
                | otherwise   -> ((n, b) :) . concat . toList $
                    go (n : ns) <$> unfix b


------------------------------------------------------------------------------------------
--  Classes
------------------------------------------------------------------------------------------

class Flow a where
    flowTypeInfo :: Proxy a -> FlowTypeInfo

    default
        flowTypeInfo :: (Generic a, GFlow (Rep a)) => Proxy a -> FlowTypeInfo
    flowTypeInfo = genericFlowType defaultOptions


genericFlowType :: forall a. (Generic a, GFlow (Rep a))
                => Options -> Proxy a -> FlowTypeInfo
genericFlowType opts _ = gFlowType opts (from (undefined :: a))



-- Primative instances
instance Flow Int where
    flowTypeInfo _ = primNumber

instance Flow Int64 where
    flowTypeInfo _ = primNumber

instance Flow Float where
    flowTypeInfo _ = primNumber

instance Flow Double where
    flowTypeInfo _ = primNumber

instance Flow Bool where
    flowTypeInfo _ = primNumber

instance Flow Text where
    flowTypeInfo _ = primString

instance Flow UTCTime where
    flowTypeInfo _ = primString

instance Flow Day where
    flowTypeInfo _ = primString

instance Flow LocalTime where
    flowTypeInfo _ = primString

instance Flow NoContent where
    flowTypeInfo _ = primVoid


instance Flow a => Flow (Maybe a) where
    flowTypeInfo _ = Fix . L1 . Nullable $ flowTypeInfo (Proxy @a)

instance Flow a => Flow [a] where
    flowTypeInfo _ = Fix . L1 . Array $ flowTypeInfo (Proxy @a)

instance Flow a => Flow (NonEmpty a) where
    flowTypeInfo _ = Fix . L1 . Array $ flowTypeInfo (Proxy @a)

instance (Ord a, Flow a) => Flow (Set a) where
    flowTypeInfo _ = Fix . L1 . Array $ flowTypeInfo (Proxy @a)

instance (Ord k, FlowObjectKey k, Flow a) => Flow (Map k a) where
    flowTypeInfo _ = Fix . L1 . Object $
        [IndexerProperty primString $ flowTypeInfo (Proxy @a)]


-- | Arbitrary types cannot be object keys but need string representation
--   "Data.Aeson.ToJSONKey" has more complex functionality than we support.
class FlowObjectKey a

instance FlowObjectKey String
instance FlowObjectKey Text


-- Generic instances
class GFlow f where
    gFlowType :: Options -> f x -> FlowTypeInfo

-- Single-constructor records
instance (GFlowRecordFields f, Datatype m1) => GFlow (D1 m1 (C1 m2 f)) where
    gFlowType opts _
        = named (T.pack $ datatypeName (undefined :: D1 m1 (C1 m2 f) ()))
        . Fix . L1 . ExactObject
        . fmap (first $ fromString . fieldLabelModifier opts)
        $ recordFields (undefined :: f ())

-- Simple sum types
instance (GSimpleSum (f :+: g), Datatype m) => GFlow (D1 m (f :+: g)) where
    gFlowType opts _
        = named (T.pack $ datatypeName (undefined :: D1 m (f :+: g) ()))
        . Fix . L1 . Sum . fmap (Fix . L1 . Literal . LitString)
        $ simpleSumOptions opts (undefined :: (f :+: g) ())

-- Use an instance that already exists
instance Flow a => GFlow (K1 i a) where
    gFlowType _ _ = flowTypeInfo (Proxy @a)

-- Record product type helper class
class GFlowRecordFields f where
    recordFields :: f x -> [(String, FlowTypeInfo)]

instance (Flow a, Selector s) => GFlowRecordFields (S1 s (K1 R a)) where
    recordFields _ =
        [(selName (undefined :: S1 s (K1 R a) ()) , flowTypeInfo (Proxy @a))]

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


------------------------------------------------------------------------------------------
--  Rendering
------------------------------------------------------------------------------------------


data Rendering = Flattened | Referenced

renderType :: Rendering -> FlowTypeInfo -> Text
renderType Flattened  = renderFlowType . forgetNames
renderType Referenced = renderFlowTypeWithReferences . toReferenced


showLiteral :: Lit -> Text
showLiteral (LitString txt) = fromString $ show txt

inParens :: Text -> Text
inParens t = "(" <> t <> ")"

inBrackets :: Text -> Text
inBrackets t = "{ " <> t <> " }"

inSuperBrackets :: Text -> Text
inSuperBrackets t = "{| " <> t <> " |}"


renderFlowType :: FlowType -> Text
renderFlowType = cata renderFlowTypeF

renderFlowTypeF :: FlowTypeF Text -> Text
renderFlowTypeF (Prim prim)     = renderPrimative prim
renderFlowTypeF (Nullable t)    = "?" <> inParens t
renderFlowTypeF (Array a)       = inParens a <> "[]"
renderFlowTypeF (Sum l)         = T.intercalate " | " l
renderFlowTypeF (Literal lit)   = showLiteral lit
renderFlowTypeF (Object ps)     = inBrackets . T.intercalate ", " $ renderProperty <$> ps
renderFlowTypeF (ExactObject l) = inSuperBrackets . T.intercalate ", " $
    (\(n, t) -> n <> " : " <> t) <$> l

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

renderFlowTypeWithReferences :: FlowTypeRef -> Text
renderFlowTypeWithReferences = cata renderFlowTypeRefF

renderFlowTypeRefF :: (FlowTypeF :+: Ref) Text -> Text
renderFlowTypeRefF (L1 ty)      = renderFlowTypeF ty
renderFlowTypeRefF (R1 (Ref n)) = n
