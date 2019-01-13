{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RON.Schema.EDN (readSchema) where

import           Control.Monad (unless)
import           Control.Monad.State.Strict (MonadState, execStateT, get, put)
import           Data.Attoparsec.Lazy (Result (Done))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Char (isSpace)
import           Data.EDN (Tagged (NoTag, Tagged), Value (List, Map, Symbol),
                           (.!=), (.:?))
import           Data.EDN.Encode (fromTagged)
import           Data.EDN.Parser (parseBSL)
import           Data.EDN.Types (EDNList, EDNMap)
import           Data.EDN.Types.Class (Parser, parseEither, typeMismatch)
import           Data.Foldable (for_, traverse_)
import           Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextL
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as TextL

import           RON.Util (ByteStringL)

import           RON.Schema

readSchema :: Monad m => String -> m (Schema 'Resolved)
readSchema source = do
    parsed <- parseSchema source
    env <- (`execStateT` Env{userTypes=Map.empty}) $ do
        collectDeclarations parsed
        validateTypeUses    parsed
    pure $ evalSchema env

newtype Env = Env{userTypes :: Map TypeName (Declaration 'Parsed)}
    deriving (Show)

data RonTypeF = Type0 RonType | Type1 (RonType -> RonType)

prelude :: Map TypeName RonTypeF
prelude = Map.fromList
    [ ("Boole",
        Type0 $
        opaqueAtoms "Boole" OpaqueAnnotations{oaHaskellType = Just "Bool"})
    , ("Day",           Type0 day)
    , ("Integer",       Type0 $ TAtom TAInteger)
    , ("RgaString",     Type0 $ TObject $ TRga char)
    , ("String",        Type0 $ TAtom TAString)
    , ("VersionVector", Type0 $ TObject TVersionVector)
    , ("Option",        Type1 $ TComposite . TOption)
    , ("ORSet",         Type1 $ TObject . TORSet)
    ]
  where
    char = opaqueAtoms "Char" OpaqueAnnotations{oaHaskellType = Just "Char"}
    day = opaqueAtoms_ "Day"

parseDeclaration :: Tagged Value -> Parser (Declaration 'Parsed)
parseDeclaration = withNoTag $ withList "declaration" $ \case
    func : args -> go =<< parseText "declaration name" func
      where
        go = \case
            "enum"       -> DEnum      <$> parseEnum      args
            "opaque"     -> DOpaque    <$> parseOpaque    args
            "struct_lww" -> DStructLww <$> parseStructLww args
            name         -> fail $ "unknown declaration " ++ Text.unpack name
    [] -> fail "empty declaration"

parseEnum :: EDNList -> Parser TEnum
parseEnum = \case
    name : items -> Enum
        <$> parseText "enum name" name
        <*> traverse (parseText "enum item") items
    [] -> fail
        "Expected declaration in the form\
        \ (enum <name:symbol> <item:symbol>...)"

parseOpaque :: EDNList -> Parser Opaque
parseOpaque = \case
    kind : name : annotations ->
        parseText "opaque kind" kind >>= \case
            "atoms"  -> go False
            "object" -> go True
            _        -> fail "opaque kind must be either atoms or object"
        where
        go isObject =
            Opaque
                isObject
                <$> parseText "opaque name" name
                <*> parseAnnotations
        parseAnnotations = case annotations of
            [] -> pure defaultOpaqueAnnotations
            _  -> fail "opaque annotations are not implemented yet"
    _ -> fail
        "Expected declaration in the form\
        \ (opaque <kind:symbol> <name:symbol> <annotations>...)"

rememberDeclaration :: MonadState Env m => Declaration 'Parsed -> m ()
rememberDeclaration decl = do
    env@Env{userTypes} <- get
    if name `Map.member` userTypes then
        fail $ "duplicate declaration of type " ++ Text.unpack name
    else
        put env {userTypes = Map.insert name decl userTypes}
  where
    name = declarationName decl

declarationName :: Declaration stage -> TypeName
declarationName = \case
    DEnum      Enum     {enumName  } -> enumName
    DOpaque    Opaque   {opaqueName} -> opaqueName
    DStructLww StructLww{structName} -> structName

parseStructLww :: EDNList -> Parser (StructLww 'Parsed)
parseStructLww = \case
    name : body -> do
        let (annotations, fields) = span isTagged body
        StructLww
            <$> parseText "struct_lww name" name
            <*> parseFields fields
            <*> parseAnnotations annotations
    [] -> fail
        "Expected declaration in the form\
        \ (struct_lww <name:symbol> <annotations>... <fields>...)"

  where

    parseFields = \case
        [] -> pure mempty
        nameAsTagged : typeAsTagged : cont -> do
            name <- parseText "struct_lww field name" nameAsTagged
            typ  <- parseTypeExpr typeAsTagged
            Map.insert name (Field typ) <$> parseFields cont
        [f] -> fail $ "field " ++ showEdn f ++ " must have type"

    parseAnnotations annTaggedValues = do
        annValues <- traverse unwrapTag annTaggedValues
        case lookup "haskell" annValues of
            Nothing -> pure defaultStructAnnotations
            Just annValue ->
                withMap "struct_lww haskell annotations map" go annValue
      where
        unwrapTag = \case
            Tagged value prefix tag -> let
                name | BS.null prefix = tag | otherwise = prefix <> "/" <> tag
                in pure (name, value)
            NoTag _ -> fail "annotation must be a tagged value"
        go m =
            StructAnnotations
            <$>  m .:? Symbol "" "field_prefix" .!= ""
            <*> (m .:? Symbol "" "field_case" >>= traverse parseCaseTransform)

parseCaseTransform :: Tagged Value -> Parser CaseTransform
parseCaseTransform v =
    parseText "case transformation" v >>= \case
        "title" -> pure TitleCase
        _       -> fail "unknown case transformation"

parseSchema :: Monad m => String -> m (Schema 'Parsed)
parseSchema string = either fail pure $ do
    values <- parseEdnStream $ encodeUtf8L string
    parseEither (traverse parseDeclaration) values

parseEdnStream :: ByteStringL -> Either String EDNList
parseEdnStream input
    | BSLC.all isSpace input = pure []
    | otherwise              =
        case parseBSL input of
            Done rest value -> (value :) <$> parseEdnStream rest
            failure         -> Left $ show failure

parseTypeExpr :: Tagged Value -> Parser TypeExpr
parseTypeExpr = withNoTag $ \case
    Symbol "" name -> pure $ Use (Text.decodeUtf8 name)
    Symbol _ _ -> fail "types must not be prefixed"
    List values -> do
        exprs <- traverse parseTypeExpr values
        case exprs of
            []       -> fail "empty type expression"
            f : args -> case f of
                Use typ -> pure $ Apply typ args
                Apply{} -> fail "type function must be a name, not expression"
    value -> typeMismatch "type symbol or expression" value

collectDeclarations :: MonadState Env m => Schema 'Parsed -> m ()
collectDeclarations = traverse_ rememberDeclaration

validateTypeUses :: MonadState Env m => Schema 'Parsed -> m ()
validateTypeUses = traverse_ $ \case
    DEnum      _                       -> pure ()
    DOpaque    _                       -> pure ()
    DStructLww StructLww{structFields} ->
        for_ structFields $ \(Field typeExpr) -> validateExpr typeExpr
  where
    validateName name = do
        Env{userTypes} <- get
        unless
            (name `Map.member` userTypes || name `Map.member` prelude)
            (fail $ "unknown type name " ++ Text.unpack name)
    validateExpr = \case
        Use name -> validateName name
        Apply name args -> do
            validateName name
            for_ args validateExpr

evalSchema :: Env -> Schema 'Resolved
evalSchema env = fst <$> userTypes' where
    Env{userTypes} = env
    userTypes' = evalDeclaration <$> userTypes

    evalDeclaration :: Declaration 'Parsed -> (Declaration 'Resolved, RonTypeF)
    evalDeclaration = \case
        DEnum   t -> (DEnum t, Type0 $ TComposite $ TEnum t)
        DOpaque t -> (DOpaque t, Type0 $ TOpaque t)
        DStructLww StructLww{..} -> let
            structFields' =
                (\(Field typeExpr) -> Field $ evalType typeExpr)
                <$> structFields
            struct = StructLww{structFields = structFields', ..}
            in (DStructLww struct, Type0 $ TObject $ TStructLww struct)

    getType typ = fromMaybe (snd $ userTypes' ! typ) $ prelude !? typ

    evalType = \case
        Use   typ      -> case getType typ of
            Type0 t0 -> t0
            Type1 _  -> error "type arity mismatch"
        Apply typ args -> applyType typ $ evalType <$> args

    applyType name args = case getType name of
        Type0 _  -> error "type arity mismatch"
        Type1 t1 -> case args of
            [a] -> t1 a
            _   -> error
                $   Text.unpack name ++ " expects 1 argument, got "
                ++  show (length args)

-- * Parser helpers

withNoPrefix
    :: Monad m
    => String -> (ByteString -> m a) -> ByteString -> ByteString -> m a
withNoPrefix ctx f prefix name = do
    unless (prefix == "") $ fail $ ctx ++ ": empty prefix expected"
    f name

withList :: String -> (EDNList -> Parser a) -> Value -> Parser a
withList expected f = \case
    List list -> f list
    value     -> typeMismatch expected value

withMap :: String -> (EDNMap -> Parser a) -> Value -> Parser a
withMap expected f = \case
    Map m -> f m
    value -> typeMismatch expected value

withNoTag :: Monad m => (Value -> m a) -> Tagged Value -> m a
withNoTag f = \case
    NoTag value         -> f value
    Tagged _ prefix tag -> fail
        $ "when expecting a non-tagged value, encountered tag "
        ++ decodeUtf8 prefix ++ "/" ++ decodeUtf8 tag ++ " instead"

withSymbol
    :: String -> (ByteString -> ByteString -> Parser a) -> Value -> Parser a
withSymbol expected f = \case
    Symbol prefix symbol -> f prefix symbol
    value                -> typeMismatch expected value

parseText :: String -> Tagged Value -> Parser Text
parseText name =
    withNoTag $ withSymbol (name ++ " symbol") $ withNoPrefix name $
    pure . Text.decodeUtf8

-- * ByteString helpers

decodeUtf8 :: ByteString -> String
decodeUtf8 = Text.unpack . Text.decodeUtf8

encodeUtf8L :: String -> ByteStringL
encodeUtf8L = TextL.encodeUtf8 . TextL.pack

-- * EDN helpers

isTagged :: Tagged a -> Bool
isTagged = \case
    NoTag {} -> False
    Tagged{} -> True

showEdn :: Tagged Value -> String
showEdn = TextL.unpack . toLazyText . fromTagged