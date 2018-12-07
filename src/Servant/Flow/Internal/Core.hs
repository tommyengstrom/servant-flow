{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Flow.Internal.Core

    ( module Servant.Flow.Internal.Core
    , Fix (..)
    , (:+:) (..)

) where

import           Data.Foldable         (toList)
import           Data.Functor.Foldable
import           Data.Monoid           ((<>))
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics


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
    | Promise a
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
newtype Lit
    = LitString Text
    deriving (Show, Eq)

-- | Object field types
data PropertyF a
    -- | A regular object field
    = Property Text a
    -- | A field which may be omitted
    | OptionalProperty Text a
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
dropAllNames :: FlowTypeInfo -> FlowType
dropAllNames = cata dropNamesF
    where
        dropNamesF :: (FlowTypeF :+: Named) FlowType -> FlowType
        dropNamesF (L1 ty) = Fix ty
        dropNamesF (R1 r)  = namedBody r

dropTypeName :: FlowTypeInfo -> FlowTypeInfo
dropTypeName (Fix (L1 ty)) = Fix (L1 ty)
dropTypeName (Fix (R1 ty)) = namedBody ty

getTypeName :: FlowTypeInfo -> Maybe Text
getTypeName (Fix (R1 n)) = Just $ namedName n
getTypeName _            = Nothing

renameType :: Text -> FlowTypeInfo -> FlowTypeInfo
renameType newName (Fix (R1 n)) = Fix . R1 $ n {namedName = newName}
renameType _       fty          = fty


-- | A Reference to a defined flow type. Differs from 'Named' in that it does not also
--   keep the value of the referenced type expression.
newtype Ref a = Ref { unRef :: Text } deriving (Functor, Show)

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
getEnv = fmap (fmap toReferenced) . go []
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
--  Rendering
------------------------------------------------------------------------------------------


data Rendering = Flattened | Referenced

renderType :: Rendering -> FlowTypeInfo -> Text
renderType Flattened  = renderFlowType . dropAllNames
renderType Referenced = renderFlowTypeWithReferences . toReferenced


showLiteral :: Lit -> Text
showLiteral (LitString txt) = fromString $ show txt

inParens :: Text -> Text
inParens t = "(" <> t <> ")"

inBrackets :: Text -> Text
inBrackets t = "{ " <> t <> " }"

inBananaBrackets :: Text -> Text
inBananaBrackets t = "{| " <> t <> " |}"


renderFlowType :: FlowType -> Text
renderFlowType = cata renderFlowTypeF

renderFlowTypeF :: FlowTypeF Text -> Text
renderFlowTypeF (Prim prim)     = renderPrimative prim
renderFlowTypeF (Nullable t)    = "?" <> inParens t
renderFlowTypeF (Array a)       = inParens a <> "[]"
renderFlowTypeF (Sum l)         = T.intercalate " | " l
renderFlowTypeF (Literal lit)   = showLiteral lit
renderFlowTypeF (Object ps)     = inBrackets . T.intercalate ", " $ renderProperty <$> ps
renderFlowTypeF (ExactObject l) = inBananaBrackets . T.intercalate ", " $
    (\(n, t) -> n <> " : " <> t) <$> l
renderFlowTypeF (Promise t)     = "Promise<" <> t <> ">"

renderPrimative :: PrimType -> Text
renderPrimative Boolean   = "boolean"
renderPrimative Number    = "number"
renderPrimative String    = "string"
renderPrimative Any       = "any"
renderPrimative AnyObject = "{}" -- unclear if/how this differs from "Object"
renderPrimative Void      = "void"

renderProperty :: PropertyF Text -> Text
renderProperty (Property fieldName ty)    = fieldName <> ": " <> ty
renderProperty (OptionalProperty key ty)  = key <> "?: " <> ty
renderProperty (IndexerProperty keyTy ty) = "[" <> keyTy <> "]: " <> ty

renderFlowTypeWithReferences :: FlowTypeRef -> Text
renderFlowTypeWithReferences = cata renderFlowTypeRefF

renderFlowTypeRefF :: (FlowTypeF :+: Ref) Text -> Text
renderFlowTypeRefF (L1 ty)      = renderFlowTypeF ty
renderFlowTypeRefF (R1 (Ref n)) = n
