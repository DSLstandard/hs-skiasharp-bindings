module Skia.Types.Internal.Utils.TH (
    qGenerateSKEnum,
    qGenerateSKObject,
    qGenerateFakeSKObject,
)
where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.Text qualified as T
import Language.Haskell.Exts.Extension qualified as Extension
import Language.Haskell.Exts.Parser
import Language.Haskell.Meta.Parse
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH
import NeatInterpolation
import Skia.Types.Core

appsT :: TH.Type -> [TH.Type] -> TH.Type
appsT = foldl AppT

mkPromotedListT :: [Type] -> Type
mkPromotedListT [] = PromotedNilT
mkPromotedListT (t : ts) = appsT PromotedConsT [t, mkPromotedListT ts]

defParseMode :: ParseMode
defParseMode =
    defaultParseMode
        { extensions = map Extension.EnableExtension exts
        }
  where
    exts =
        [ Extension.TypeFamilies
        , Extension.MultiParamTypeClasses
        ]

parseSingleDecWithMode :: ParseMode -> String -> Either String Dec
parseSingleDecWithMode parseMode source = do
    result <- parseDecsWithMode parseMode source
    case result of
        [dec] -> do
            pure dec
        decs -> do
            Left $ "expects 1 Dec is returned, but got " <> show (length decs) <> " decs"

parseSourceOrDie :: (String -> Either String a) -> T.Text -> Q a
parseSourceOrDie parserFunc source = do
    let result = parserFunc (T.unpack source)
    case result of
        Left errMsg -> do
            error errMsg
        Right result -> do
            pure result

renderType :: Type -> T.Text
renderType ty = T.pack $ pprint $ ParensT ty

-- | Helper structure used by 'qGenerateSKEnum'
data EnumValueEntry = EnumValueEntry
    { hsValueName :: Name
    , cValueName :: Name
    , docstring :: T.Text
    }

-- NOTE: This function is intentionally kept.
--
-- renderName :: Name -> T.Text
-- renderName n = T.pack $ pprint n

{- | Generates 1) the data declaration, and 2) instance SKEnum of a C enum type.

Example usage:

@
\$( qGenerateSKEnum
    \"SKPointMode\"
    ''Sk_point_mode
    "Docstring about Sk_point_mode"
    [ (\"Points\", 'POINTS_SK_POINT_MODE, \"Docstring for SKPointMode'Points\")
    , (\"Lines\", 'LINES_SK_POINT_MODE, \"\")
    , (\"Polygon\", 'POLYGON_SK_POINT_MODE, \"\")
    ]
 )
@

... generates the following:

@
-- | Docstring about Sk_point_mode
data SKPointMode
    = SKPointMode'Points -- ^ Docstring for SKPointMode'Points
    | SKPointMode'Lines
    | SKPointMode'Polygon
    deriving (Show, Eq, Ord, Enum, Bounded)

instance SKEnum where
    marshalSKEnum SKPointMode'Points = POINTS_SK_POINT_MODE
    marshalSKEnum SKPointMode'Lines = LINES_SK_POINT_MODE
    marshalSKEnum SKPointMode'Polygon = POLYGON_SK_POINT_MODE

    unmarshalSKEnum POINTS_SK_POINT_MODE = Just SKPointMode'Points
    unmarshalSKEnum LINES_SK_POINT_MODE = Just SKPointMode'Lines
    unmarshalSKEnum POLYGON_SK_POINT_MODE = Just SKPointMode'Polygon
    unmarshalSKEnum _ = Nothing
@
-}
qGenerateSKEnum ::
    -- | Haskell enum name string
    String ->
    -- | Bindings enum name
    Name ->
    -- | Optional docstring of the enum datatype (use "" to indicate no
    -- docstring))
    T.Text ->
    -- | Pairs of (Haskell enum value name string, Bindings enum value name,
    -- Optional docstring of the enum value (use "" to indicate no docstring))
    [(String, Name, T.Text)] ->
    DecsQ
qGenerateSKEnum hsEnumNameStr cEnumName datatypeDocstring inPairs = do
    let
        hsEnumName = mkName hsEnumNameStr
        entries =
            inPairs <&> \(hsValueNameStr, cValueName, docstring) ->
                EnumValueEntry
                    { hsValueName = mkName (hsEnumNameStr <> "'" <> hsValueNameStr)
                    , cValueName
                    , docstring
                    }

    let
        enumDerivingStock =
            DerivClause
                (Just StockStrategy)
                [ConT (mkName n) | n <- ["Show", "Eq", "Ord", "Enum", "Bounded"]]
        enumCons = do
            EnumValueEntry{hsValueName} <- entries
            pure $ NormalC hsValueName []
        enumAdtDec = DataD [] hsEnumName [] Nothing enumCons [enumDerivingStock]

    let
        skEnumType = appsT (ConT ''SKEnum) [ConT hsEnumName, ConT cEnumName]

        skEnumMarshal = FunD 'marshalSKEnum do
            EnumValueEntry{hsValueName, cValueName} <- entries
            pure $ Clause [ConP hsValueName [] []] (NormalB (ConE cValueName)) []

        skEnumUnmarshal = FunD 'unmarshalSKEnum do
            EnumValueEntry{hsValueName, cValueName} <- entries
            pure $ Clause [ConP cValueName [] []] (NormalB (ConE 'Just `AppE` ConE hsValueName)) []

        skEnumInstanceDec = InstanceD Nothing [] skEnumType [skEnumMarshal, skEnumUnmarshal]

    -- ### Add docstrings
    --
    -- The generated definitions must be in-scope first, so addModFinalizer is
    -- necessary.
    TH.addModFinalizer do
        -- Docstrings for the enum type
        unless (T.null datatypeDocstring) do
            TH.putDoc (TH.DeclDoc hsEnumName) (T.unpack datatypeDocstring)

        -- Docstrings for enum values
        for_ entries \EnumValueEntry{hsValueName, docstring} -> do
            unless (T.null docstring) do
                TH.putDoc (TH.DeclDoc hsValueName) (T.unpack docstring)

    pure [enumAdtDec, skEnumInstanceDec]

createNewType :: T.Text -> T.Text -> Type -> DecQ
createNewType name project referencedType = do
    let ty = renderType referencedType
    parseSourceOrDie
        (parseSingleDecWithMode defParseMode)
        [text|
            newtype ${name} = ${name}
                { ${project} :: ManagedPtr ${ty}
                }
                deriving (Show)
        |]

createManagedPtrNewTypeInstance :: T.Text -> T.Text -> Type -> DecQ
createManagedPtrNewTypeInstance name project aType = do
    let ty = renderType aType
    parseSourceOrDie
        (parseSingleDecWithMode defParseMode)
        [text|
            instance ManagedPtrNewType ${name} ${ty} where
                fromManagedPtr = ${name}
                toManagedPtr = ${project}
        |]

createSKObjectInstance :: T.Text -> T.Text -> DecQ
createSKObjectInstance name project = do
    parseSourceOrDie
        (parseSingleDecWithMode defParseMode)
        [text|
            instance SKObject ${name} where
                fromAnyManagedPtr = ${name} . castManagedPtr
                toAnyManagedPtr = castManagedPtr . ${project}
        |]

{- | Example usage:

@
createClassExtends 'SKStreamRewindable ['SKStream])
@

... generates the following:

@
type instance ParentTypes SKStreamRewindable = '[SKStream]
type IsSKStreamRewindable = IsSubclassOf SKStreamRewindable
@
-}
createClassExtends ::
    -- | Class
    Name ->
    -- | Parents. Can be empty.
    [Name] ->
    DecsQ
createClassExtends cls parents = do
    -- The following code does not work and I don't know how to fix it.
    --
    -- The parser fails with "Improper character constant or misplaced '"
    --
    -- ```
    -- let cls = renderName clsName
    -- let parents = "'[" <> T.intercalate ", " (map renderName parentNames) <> "]"
    -- parseSourceOrDie
    --     (parseDecsWithMode defParseMode)
    --     [text|
    --         type instance ParentTypes ${cls} = ${parents}
    --         type Is${cls} = IsSubclassOf ${cls}
    --     |]
    -- ```

    let
        parentTypesDec =
            TySynInstD (TySynEqn Nothing lhs rhs)
          where
            lhs = appsT (ConT ''ParentTypes) [ConT cls]
            rhs = mkPromotedListT [ConT parent | parent <- parents]

    let
        isTypeName = mkName ("Is" <> nameBase cls)
        isTypeAliasDec =
            TySynD
                isTypeName
                [PlainTV (mkName "a") ()]
                ( appsT
                    (ConT ''IsSubclassOf)
                    [ ConT cls
                    , VarT (mkName "a")
                    ]
                )

    pure
        [ parentTypesDec
        , isTypeAliasDec
        ]

qGenerateSKObject ::
    -- | Haskell object name string
    String ->
    -- | Bindings object name
    Name ->
    -- | Superclasses
    [Name] ->
    -- | Optional docstring of the enum datatype (use "" to indicate no
    -- docstring))
    T.Text ->
    DecsQ
qGenerateSKObject nameStr cObjName superClasses docstring = do
    let name = T.pack nameStr
    let project = "un" <> name

    -- ### Add docstrings
    --
    -- The generated definitions must be in-scope first, so addModFinalizer is
    -- necessary.
    TH.addModFinalizer do
        -- Docstrings for the newtype declaration
        unless (T.null docstring) do
            TH.putDoc (TH.DeclDoc (mkName nameStr)) (T.unpack docstring)

    sequenceA
        [ createNewType name project (ConT cObjName)
        , createManagedPtrNewTypeInstance name project (ConT cObjName)
        , createSKObjectInstance name project
        ]
        <> createClassExtends (mkName nameStr) superClasses

{- | Generates a "fake" SKObject. Specifically:

@
\$(qGenerateFakeSKObject \"SKStreamAsset\" [''SKStreamAsset])
@

... generates the following:

@
newtype SKStreamMemory = SKStreamAsset
    { unSKStreamMemory :: ManagedPtr ()
        -- NOTE: The referenced type is unimportant. We put () here as a
        -- placeholder.
    }

-- NOTE: Compared to \'qGenerateSKObject\', \'qGenerateFakeSKObject\' does not
-- generate a \'ManagedPtrNewType\' instance.
--
-- This prevents use 'Skia.Types.Core.useObj' on fake SKObjects because
-- there is never a corresponding Mono Skia C type.

instance SKObject SKStreamMemory where
    ...

type instance ParentTypes SKStreamMemory = '[SKStreamAsset]
type IsSKStreamMemory = IsSubclassOf SKStreamMemory
@
-}
qGenerateFakeSKObject ::
    -- | Haskell object name string
    String ->
    -- | Superclasses
    [Name] ->
    -- | Optional docstring of the enum datatype (use "" to indicate no
    -- docstring))
    T.Text ->
    DecsQ
qGenerateFakeSKObject nameStr superClasses docstring = do
    let name = T.pack nameStr
    let project = "un" <> name

    -- ### Add docstrings
    --
    -- The generated definitions must be in-scope first, so addModFinalizer is
    -- necessary.
    TH.addModFinalizer do
        -- Docstrings for the newtype declaration
        unless (T.null docstring) do
            TH.putDoc (TH.DeclDoc (mkName nameStr)) (T.unpack docstring)

    sequenceA
        [ createNewType name project (TupleT 0)
        , createSKObjectInstance name project
        ]
        <> createClassExtends (mkName nameStr) superClasses