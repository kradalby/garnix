module Garnix.API.Keys where

import Data.Text qualified as T
import Garnix.DB qualified as DB
import Garnix.Monad
import Garnix.Monad.SubProcess.Deprecated qualified as Deprecated
import Garnix.Prelude
import Garnix.Types

getRepoPublicKey :: RepoOwner -> RepoName -> M PublicKey
getRepoPublicKey owner name = fst <$> getRepoKeys owner name

getActionPublicKey :: RepoOwner -> RepoName -> PackageName -> M PublicKey
getActionPublicKey owner name action = fst <$> getActionKeys owner name action

getRepoKeys :: RepoOwner -> RepoName -> M (PublicKey, PrivateKey)
getRepoKeys owner name = do
  mkey <- DB.getRepoKeyDB owner name
  case mkey of
    Nothing -> do
      (candidatePubKey, candidatePrivKey) <- generateKeys
      DB.setRepoKeyDB owner name candidatePubKey candidatePrivKey
    Just key -> pure key

getActionKeys :: RepoOwner -> RepoName -> PackageName -> M (PublicKey, PrivateKey)
getActionKeys owner name action = do
  mkey <- DB.getActionKeyDB owner name action
  case mkey of
    Nothing -> do
      (candidatePubKey, candidatePrivKey) <- generateKeys
      DB.setActionKeyDB owner name action candidatePubKey candidatePrivKey
    Just key -> pure key

generateKeys :: M (Candidate PublicKey, Candidate PrivateKey)
generateKeys = do
  output <- Deprecated.runProc "age-keygen" [] []
  case T.lines output of
    [_createdAt, pubKeyLine, privKeyLine] -> do
      unless ("# public key: " `T.isPrefixOf` pubKeyLine)
        $ throw
        $ OtherError "age-keygen responded with unexpected format"
      repoSecretsPubKey <- view #repoSecretsEncryptionPubKey
      privKey <-
        liftIO (makePrivateKey (cs privKeyLine) repoSecretsPubKey) >>= \case
          Left e -> throw $ OtherError e
          Right v -> pure v
      pure
        ( Candidate (PublicKey $ T.drop (T.length "# public key: ") pubKeyLine),
          Candidate privKey
        )
    _ -> throw $ OtherError "age-keygen responded with unexpected format"
