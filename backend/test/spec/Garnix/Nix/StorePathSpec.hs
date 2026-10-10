module Garnix.Nix.StorePathSpec where

import Control.Lens ((<&>), (^?!))
import Control.Monad.Trans.Control (liftBaseOp_)
import Cradle
import Data.Aeson (fromJSON)
import Data.Aeson.Lens (key, nth, _String)
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as Map
import Data.String.Interpolate (i)
import Garnix.Nix.StorePath
import Garnix.Nix.Types (DrvPath (..), StoreHash (..), StorePath (..), parseDrvPath)
import Garnix.Nix.Types qualified as Nix
import Garnix.NixConfig (nixConfDefaults)
import Garnix.Prelude
import Garnix.TestHelpers
import Garnix.TestHelpers.Monad
import Garnix.Types
import System.Posix.Files (readSymbolicLink)
import Test.Hspec
import Test.Mockery.Environment (withModifiedEnvironment)

spec :: Spec
spec = inM $ aroundM_ suppressLogsWhenPassing $ do
  describe "parseDrvPath" $ do
    it "parses drv store paths" $ do
      parseDrvPath @Text "/nix/store/8a07w55ia5lffnpww5jfi1lx0yg6hk1k-garnix.drv"
        `shouldBeM` Right (DrvPath (StorePath (StoreHash "8a07w55ia5lffnpww5jfi1lx0yg6hk1k") "garnix.drv"))

    it "rejects non drv paths" $ do
      parseDrvPath @Text "/nix/store/iqi8dg7x9ygafvs3h25bx8xaxwhywip3-garnix"
        `shouldBeM` Left "not a drv path: /nix/store/iqi8dg7x9ygafvs3h25bx8xaxwhywip3-garnix"

  let mkDrvPath p = Nix.DrvPath $ either error identity (Nix.parseStorePath @Text p)
      testDerivation =
        [i|
          derivation {
            name = "test-package";
            builder = "/bin/sh";
            system = "x86_64-linux";
            outputs = [ "out" "otherOut" ];
            args = [ "-c" ''
              echo "foo" > $otherOut
              echo "bar" > $out
              echo Done
            ''];
          }
        |]

  describe "getClosure" $ do
    let missingPath = StorePath (StoreHash "00000000000000000000000000000000") "missing-closure-for-test"

    it "ignores the missing output of a failed build" $ do
      getClosure missingPath `shouldReturnM` Nothing

    it "ignores missing outputs when Nix emits configuration warnings" $ do
      liftBaseOp_ (withModifiedEnvironment [("NIX_CONFIG", "garnix-test-unknown-setting = true")]) $ do
        getClosure missingPath `shouldReturnM` Nothing

    it "returns the closure of a successful build" $ do
      StdoutTrimmed json <-
        run
          $ cmd "nix"
          & addArgs ["build", "--expr", testDerivation, "--json"]
          & nixConfDefaults
      let drvPath = json ^?! nth 0 . key "drvPath" . _String . to mkDrvPath
      storePath <- _getOutputs drvPath <&> (Map.! "out")
      getClosure storePath `shouldReturnM` Just [storePath]

  describe "_getOutput" $ do
    it "fetch storepath from a drv" $ do
      StdoutTrimmed o <-
        run
          $ cmd "nix"
          & addArgs
            [ "build",
              "--expr",
              testDerivation,
              "--json"
            ]
          & nixConfDefaults
      let drvPath = o ^?! nth 0 . key "drvPath" . _String . to mkDrvPath
      storePathsMap <- _getOutputs drvPath
      Map.keys storePathsMap `shouldBeM` ["otherOut", "out"]

  describe "_withGcRoot" $ do
    it "gc root existing store path" $ do
      StdoutTrimmed json <-
        run
          $ cmd "nix"
          & addArgs
            [ "build",
              "--expr",
              testDerivation,
              "--json"
            ]
          & nixConfDefaults
      let drvPath = json ^?! nth 0 . key "drvPath" . _String . to mkDrvPath
      storePath <- _getOutputs drvPath <&> (Map.! "out")
      _withGcRoot storePath $ \gcRoot -> do
        target <- liftIO $ readSymbolicLink (cs gcRoot)
        target `shouldBeM` cs storePath

  describe "withStorePath" $ do
    it "fetch storepath from a drv" $ do
      StdoutTrimmed o <-
        run
          $ cmd "nix"
          & addArgs
            [ "build",
              "--expr",
              testDerivation,
              "--json"
            ]
          & nixConfDefaults
      let drv = o ^?! nth 0 . key "drvPath" . _String . to mkDrvPath
      build <- testBuild (drvPath ?~ cs drv)
      withStorePath build "out" $ \storePath -> do
        outStorePath <- _getOutputs drv <&> (Map.! "out")
        storePath `shouldBeM` Just outStorePath

    it "uses build outputs if they are set" $ do
      StdoutTrimmed o <-
        run
          $ cmd "nix"
          & addArgs
            [ "build",
              "--expr",
              testDerivation,
              "--json"
            ]
          & nixConfDefaults
      let outputs :: BuildOutputsPgColumn = case fromJSON $ o ^?! nth 0 . key "outputs" of
            Aeson.Error err -> error $ cs err
            Aeson.Success outputs -> outputs
      build <- testBuild $ (drvPath ?~ "/nix/store/some-invalid-drv-path-should-be-unused") . (outputPaths ?~ outputs)
      withStorePath build "out" $ \storePath -> do
        let Just expectedStorePath = Nix.getOutputByName "out" $ buildOutputs outputs
        storePath `shouldBeM` Just expectedStorePath
