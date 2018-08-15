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

instance GFlowRecordFields f => GFlowTyped (M1 D meta f) where
    gFlowType _ = Fix . ExactObject $ recordFields (undefined :: f ())

instance FlowTyped a => GFlowTyped (K1 i a) where
    gFlowType _ = flowType (Proxy @a)


class GFlowRecordFields f where
    recordFields :: f x -> [(Text, FlowType)]

instance (FlowTyped a, Selector s) => GFlowRecordFields (M1 S s (K1 R a)) where
    recordFields _ =
        [(fromString $ selName (undefined :: M1 S s (K1 R a) ()) , flowType (Proxy @a))]

instance (GFlowRecordFields f, GFlowRecordFields g) => GFlowRecordFields (f :*: g) where
    recordFields _ = recordFields (undefined :: f ()) <> recordFields (undefined :: g ())

instance GFlowRecordFields f => GFlowRecordFields (M1 C meta f) where
    recordFields _ = recordFields $ (undefined :: f ())


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
    deriving (Show, Eq, Functor, Traversable, Foldable)


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
showFlowTypeF (ExactObject l) = inSuperBrackets $
    T.intercalate ", " (fmap (\(n, t) -> n <> " : " <> t) l)
