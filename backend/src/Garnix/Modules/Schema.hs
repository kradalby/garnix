{-# LANGUAGE TemplateHaskell #-}

module Garnix.Modules.Schema where

import Cradle
import Data.Aeson hiding ((.?=))
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Garnix.DB.ModuleValues qualified as ModuleValues
import Garnix.Monad
import Garnix.NixConfig (addNixConfigEnvironment)
import Garnix.Prelude
import Garnix.Sandbox
import Garnix.Types hiding (description)
import Text.Casing qualified as Casing

data ModuleSchema = ModuleSchema
  { description :: Maybe Text,
    example :: Maybe Text,
    default_ :: Maybe ModuleValues.NixValue,
    name :: Maybe Text,
    typ :: ModuleSchemaType
  }
  deriving (Show, Eq)

instance ToJSON ModuleSchema where
  toJSON (ModuleSchema {description, example, default_, name, typ}) =
    object
      ( ("description" .?= description)
          <> ("example" .?= example)
          <> ("default" .?= default_)
          <> ("name" .?= name)
          <> ["typ" .= typ]
      )

(.?=) :: (ToJSON a) => Key -> Maybe a -> [(Key, Value)]
name .?= value = case value of
  Nothing -> []
  Just value -> [(name, toJSON value)]

instance FromJSON ModuleSchema where
  parseJSON = withObject "ModuleSchema" $ \o -> do
    typ <- o .: "typ"
    description <- o .:? "description"
    example <- o .:? "example"
    default_ <- o .:? "default"
    name <- o .:? "name"
    pure $ ModuleSchema description example default_ name typ

data ModuleSchemaType
  = Secret
  | Path
  | Str
  | NonEmptyStr
  | Bool
  | Int
  | UnsignedInt16
  | Enum [Text]
  | Package
  | Submodule (Map Text ModuleSchema)
  | AttrsOf ModuleSchemaType
  | ListOf ModuleSchemaType
  | NullOr ModuleSchemaType
  deriving (Show, Eq)

instance ToJSON ModuleSchemaType where
  toJSON = \case
    Secret -> object ["tag" .= ("encryptedSecret" :: Text)]
    Path -> object ["tag" .= ("path" :: Text)]
    Str -> object ["tag" .= ("str" :: Text)]
    NonEmptyStr -> object ["tag" .= ("nonEmptyStr" :: Text)]
    Garnix.Modules.Schema.Bool -> object ["tag" .= ("bool" :: Text)]
    Garnix.Modules.Schema.Int -> object ["tag" .= ("int" :: Text)]
    UnsignedInt16 -> object ["tag" .= ("unsignedInt16" :: Text)]
    Garnix.Modules.Schema.Enum variants -> object ["tag" .= ("enum" :: Text), "variants" .= variants]
    Garnix.Modules.Schema.Package -> object ["tag" .= ("package" :: Text)]
    Submodule fields -> object ["tag" .= ("submodule" :: Text), "fields" .= fields]
    AttrsOf elementType -> object ["tag" .= ("attrsOf" :: Text), "fieldType" .= elementType]
    ListOf elementType -> object ["tag" .= ("listOf" :: Text), "elementType" .= elementType]
    NullOr innerType -> object ["tag" .= ("nullOr" :: Text), "innerType" .= innerType]

instance FromJSON ModuleSchemaType where
  parseJSON = withObject "ModuleSchemaType" $ \o -> do
    tag <- o .: "tag"
    case tag of
      "encryptedSecret" -> pure Secret
      "path" -> pure Path
      "str" -> pure Str
      "nonEmptyStr" -> pure NonEmptyStr
      "bool" -> pure Garnix.Modules.Schema.Bool
      "int" -> pure Garnix.Modules.Schema.Int
      "unsignedInt16" -> pure UnsignedInt16
      "enum" -> Enum <$> o .: "variants"
      "package" -> pure Garnix.Modules.Schema.Package
      "submodule" -> Submodule <$> o .: "fields"
      "attrsOf" -> AttrsOf <$> o .: "fieldType"
      "listOf" -> ListOf <$> o .: "elementType"
      "nullOr" -> NullOr <$> o .: "innerType"
      _ -> fail $ "unknown tag: " <> tag

readModuleSchema :: FilePath -> M ModuleSchema
readModuleSchema path = do
  nixConfig <- view #userNixConfig
  (StdoutUntrimmed output, StderrRaw stderr, exitCode) <-
    cmd "nix"
      & addArgs
        [ "eval",
          path <> "#garnixModules.default",
          "--json",
          "--apply",
          [i| (#{moduleToSchemaNix}) { nixpkgsLib = #{nixpkgsLib}; } |]
        ]
      & setWorkingDir path
      & addNixConfigEnvironment nixConfig
      & pure
      & inNixSandbox [] Nothing
  when (exitCode /= ExitSuccess) $ do
    throw $ OtherError $ cs stderr
  schema <- aesonDecode "extracted module schema" parseJSON output
  StdoutUntrimmed output <-
    cmd "nix"
      & addArgs ["flake", "metadata", "--json" :: Text]
      & setWorkingDir path
      & addNixConfigEnvironment nixConfig
      & pure
      & inNixSandbox [] Nothing
  let moduleDescription = output ^? key "description" . _String
  pure $ schema {description = moduleDescription}

repoNameToModuleName :: Text -> Text
repoNameToModuleName name =
  let nameFromFile = fromMaybe name $ T.stripSuffix "-module" name
   in case nameFromFile of
        "nodejs" -> "NodeJS"
        "postgresql" -> "PostgreSQL"
        "rss-bridge" -> "RSS-Bridge"
        otherName -> cs . Casing.pascal . cs $ otherName

moduleToSchemaNix :: ByteString
moduleToSchemaNix = $(embedFileRelative "src/Garnix/Modules/module-to-schema.nix")

nixpkgsLib :: Text
nixpkgsLib =
  cs
    [i|
      (let nixpkgsLib = builtins.fetchGit (builtins.fromJSON ''#{encode args}'');
       in import "${nixpkgsLib}/lib")
    |]
  where
    args :: Map Text Text
    args =
      Map.fromList
        [ ("url", "https://github.com/nix-community/nixpkgs.lib"),
          ("ref", "master"),
          ("rev", "f4dc9a6c02e5e14d91d158522f69f6ab4194eb5b")
        ]
