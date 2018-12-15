{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Flow.Internal.Class where

import           Data.Aeson                 (Options (..), defaultOptions, SumEncoding (..))
import           Data.Bifunctor             (first)
import           Data.Functor.Foldable
import           Data.Int                   (Int64)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Map                   (Map)
import           Data.Monoid                ((<>))
import           Data.Proxy
import           Data.Set                   (Set)
import           Data.String
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time                  (Day, LocalTime, UTCTime)
import           GHC.Generics
import           Servant.API                (NoContent)
import           Servant.Flow.Internal.Core
import Data.Functor

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
    flowTypeInfo _ = primBoolean

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

-- | NOTE: This instance is potentially subject to future change
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
instance {-# OVERLAPPABLE #-} (GFlowRecordFields f, Datatype m1) => GFlow (D1 m1 (C1 m2 f)) where
    gFlowType opts _
        = named (T.pack $ datatypeName (undefined :: D1 m1 (C1 m2 f) ()))
        . Fix . L1 . ExactObject
        . fmap (first $ fromString . fieldLabelModifier opts)
        $ recordFields (undefined :: f ())

-- Simple sum types
instance {-# OVERLAPPABLE #-} (GSimpleSum cs, Datatype m) => GFlow (D1 m cs) where
    gFlowType opts _
        = named (T.pack $ datatypeName (undefined :: D1 m cs ()))
        . Fix . L1 . Sum . fmap (Fix . L1 . Literal . LitString)
        $ simpleSumOptions opts (undefined :: cs ())

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





data FieldInfo
    = AnonField FlowTypeInfo
    | RecordField String FlowTypeInfo

instance Show FieldInfo where
    show (AnonField fTy)             = unwords
        [ "AnonField"
        , T.unpack (renderType Referenced fTy)
        ]
    show (RecordField fieldName fTy) = unwords
        [ "RecordField"
        , show fieldName
        , T.unpack (renderType Referenced fTy)
        ]

data FlowConstructor = FlowConstructor [FieldInfo]


-- Constructor helper class
class GFlowField f where
    flowField :: f x -> FieldInfo

instance (Flow a, Selector s) => GFlowField (S1 s (K1 R a)) where
    flowField _ =
        RecordField
            (selName (undefined :: S1 s (K1 R a) ()))
            $ flowTypeInfo (Proxy @a)

class GFlowConstructorFields f where
    constructorFields :: f x -> [FieldInfo]

-- Rather than using UndecidableInstances here, maybe just define the instance directly
-- in terms of S1.
instance GFlowField f => GFlowConstructorFields f where
    constructorFields _ = pure @[] $ flowField (undefined :: f ())

instance (GFlowConstructorFields f, GFlowConstructorFields g)
    => GFlowConstructorFields (f :*: g) where
        constructorFields _ =
               constructorFields (undefined :: f ())
            <> constructorFields (undefined :: g ())

class GFlowConstructor f where
    constructors :: f x -> [FlowConstructor]

instance GFlowConstructorFields f => GFlowConstructor (C1 m f) where
    constructors _ = [FlowConstructor $ constructorFields (undefined :: f ()) ]

instance (GFlowConstructor f, GFlowConstructor g) => GFlowConstructor (f :+: g) where
    constructors _ = constructors (undefined :: f ())
                  <> constructors (undefined :: g ())


data FieldError
    = Unexpected [FieldInfo]
    deriving Show

data ProperConstructor
    = AnonConstructor Text [FlowTypeInfo]
    | RecordConstructor Text [(String, FlowTypeInfo)]

getConstructorName :: ProperConstructor -> Text
getConstructorName (AnonConstructor   constName _) = constName
getConstructorName (RecordConstructor constName _) = constName

data FlowDatatype = FlowDatatype [ProperConstructor]



class GFlowDatatype f where
    gFlowDatatype :: f x -> FlowDatatype

instance GFlowConstructor f => GFlowDatatype (D1 m f) where
    gFlowDatatype _ = either (\er -> error (show er)) FlowDatatype $
        traverse mkProperConstructor (constructors $ (undefined :: f ()))



mkProperConstructor :: FlowConstructor -> Either FieldError ProperConstructor
mkProperConstructor = undefined

encodeFlowUnion :: FlowDatatype -> Options -> FlowTypeInfo
encodeFlowUnion (FlowDatatype [c]) opts
    | not (tagSingleConstructors opts) = encodeFlowConstructor opts c
    -- Not "Encode types with a single constructor as sums, so that
    -- allNullaryToStringTag and sumEncoding apply."
encodeFlowUnion (FlowDatatype cs) opts
    | all nullary cs && allNullaryToStringTag opts = Fix . L1 . Sum $ Fix . L1 . Literal . LitString . getConstructorName <$> cs
    | otherwise                                    = Fix . L1 . Sum $ cs <&> \c -> case sumEncoding opts of
            TaggedObject tag contents -> Fix . L1 . ExactObject $
                case summandType of
                    (Fix (L1 (ExactObject l))) -> tagProperty c : l
                    _                          ->
                        [ tagProperty c
                        , (T.pack contents, summandType)
                        ]
                where
                    summandType = encodeFlowConstructor opts c
                    tagProperty constr
                        = (T.pack tag,)
                        . Fix . L1 . Literal . LitString
                        . constrMod
                        $ getConstructorName constr

            UntaggedValue             -> encodeFlowConstructor opts c
            ObjectWithSingleField     -> Fix . L1 $ ExactObject
                [(constrMod $ getConstructorName c, encodeFlowConstructor opts c)]
            TwoElemArray              -> Fix . L1 $ Array primAny

    where
        constrMod = T.pack . constructorTagModifier opts . T.unpack

        nullary (AnonConstructor _ [])   = True
        nullary (RecordConstructor _ []) = True
        nullary _                        = False


encodeFlowConstructor :: Options -> ProperConstructor -> FlowTypeInfo
encodeFlowConstructor opts = \case
    RecordConstructor _str [(_,ty)]
        | unwrapUnaryRecords opts  -> ty
    RecordConstructor _str tyPairs -> Fix . L1 . ExactObject $
        first (T.pack . fieldLabelModifier opts) <$> tyPairs
    AnonConstructor _str _fTypes   -> Fix . L1 . Array $ primAny
