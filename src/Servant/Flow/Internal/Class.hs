{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Flow.Internal.Class where

import           Control.Applicative        ((<|>))
import           Data.Aeson                 (Options (..), SumEncoding (..),
                                             defaultOptions)
import           Data.Bifunctor             (first)
import           Data.Functor
import           Data.Functor.Foldable
import           Data.Int                   (Int64)
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Map                   (Map)
import           Data.Monoid                ((<>))
import           Data.Proxy
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Time                  (Day, LocalTime, UTCTime)
import           GHC.Generics
import           Servant.API                (NoContent)
import           Servant.Flow.Internal.Core


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

instance Flow TL.Text where
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


data FieldInfo
    = AnonField FlowTypeInfo
    | RecordField String FlowTypeInfo

data RawConstructor = RawConstructor Text [FieldInfo]

data FieldError
    = Unexpected [FieldInfo]
    deriving Show


data ProperConstructor
    = AnonConstructor Text [FlowTypeInfo]
    | RecordConstructor Text [(String, FlowTypeInfo)]

data FlowDatatype = FlowDatatype [ProperConstructor]


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



-- Use an instance that already exists
instance (Flow a, Selector s) => GFlowConstructorFields (S1 s (K1 R a)) where
    constructorFields _ = pure $ if
        | null s    -> AnonField     $ flowTypeInfo (Proxy @a)
        | otherwise -> RecordField s $ flowTypeInfo (Proxy @a)
        where
            s = selName (undefined :: S1 s (K1 R a) ())

class GFlowConstructorFields f where
    constructorFields :: f x -> [FieldInfo]

instance GFlowConstructorFields U1 where
    constructorFields _ = []

instance (GFlowConstructorFields f, GFlowConstructorFields g)
    => GFlowConstructorFields (f :*: g) where
        constructorFields _ =
               constructorFields (undefined :: f ())
            <> constructorFields (undefined :: g ())

class GFlowConstructors f where
    constructors :: f x -> [RawConstructor]

instance (GFlowConstructorFields f, Constructor m) => GFlowConstructors (C1 m f) where
    constructors _
        = pure
        . RawConstructor (T.pack $ conName @m undefined)
        $ constructorFields (undefined :: f ())

instance (GFlowConstructors f, GFlowConstructors g) => GFlowConstructors (f :+: g) where
    constructors _ = constructors (undefined :: f ())
                  <> constructors (undefined :: g ())


instance GFlowConstructors f => GFlow (D1 m f) where
    gFlowType opts _
        = encodeFlowUnion opts
        . either (\er -> error (show er)) FlowDatatype
        $ traverse mkProperConstructor (constructors $ (undefined :: f ()))


mkProperConstructor :: RawConstructor -> Either FieldError ProperConstructor
mkProperConstructor (RawConstructor name fs) = maybe (Left $ Unexpected fs) Right $
        fmap (RecordConstructor name) (traverse requireRecordField fs)
    <|> fmap (AnonConstructor   name) (traverse requireAnonField fs)

    where
        requireAnonField :: FieldInfo -> Maybe FlowTypeInfo
        requireAnonField (AnonField ty) = Just ty
        requireAnonField _              = Nothing

        requireRecordField :: FieldInfo -> Maybe (String, FlowTypeInfo)
        requireRecordField (RecordField f ty) = Just (f, ty)
        requireRecordField _                  = Nothing



encodeFlowUnion :: Options -> FlowDatatype -> FlowTypeInfo
encodeFlowUnion opts (FlowDatatype [c])
    | not (tagSingleConstructors opts) = encodeFlowConstructor opts c
    -- Not "Encode types with a single constructor as sums, so that
    -- allNullaryToStringTag and sumEncoding apply."
encodeFlowUnion opts (FlowDatatype cs) = Fix . L1 . Sum $ cs <&> \c -> if
    | all nullary cs && allNullaryToStringTag opts -> Fix . L1 . Literal . LitString $ constrName c
    | otherwise                                    -> case sumEncoding opts of
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
                    $ constrName constr

        UntaggedValue             -> encodeFlowConstructor opts c
        ObjectWithSingleField     -> Fix . L1 $ ExactObject
            [(constrName c, encodeFlowConstructor opts c)]
        TwoElemArray              -> Fix . L1 $ Array primAny

    where
        constrName = T.pack . constructorTagModifier opts . T.unpack . getConstructorName

        getConstructorName (AnonConstructor   constName _) = constName
        getConstructorName (RecordConstructor constName _) = constName

        nullary (AnonConstructor _ [])   = True
        nullary (RecordConstructor _ []) = True
        nullary _                        = False

-- | Flow representation of a particular data constructor
encodeFlowConstructor :: Options -> ProperConstructor -> FlowTypeInfo
encodeFlowConstructor opts = \case
    RecordConstructor _str [(_,ty)]
        | unwrapUnaryRecords opts  -> ty
    RecordConstructor _str tyPairs -> Fix . L1 . ExactObject $
        first (T.pack . fieldLabelModifier opts) <$> tyPairs
    AnonConstructor _str _fTypes   -> Fix . L1 . Array $ primAny
