module EA.CLI.Auth.Run (
  runAuthCommand,
) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString.Char8 qualified as BS
import Data.Text qualified as T
import Database.Persist.Sql (runSqlPool)
import EA.CLI.Auth.Command (AuthCommand (..), CheckAuthTokenCommand (..), CreateAuthTokenCommand (..))
import EA.CLI.Helper (fetchSqlPool)
import GeniusYield.Imports (printf)
import Internal.Wallet.DB.Sql (addToken, checkToken)

runAuthCommand :: AuthCommand -> IO ()
-- Create New Auth token
runAuthCommand (CreateAuthToken (CreateAuthTokenCommand token notes)) = do
  printf "Creating Auth token"
  pool <- fetchSqlPool
  runSqlPool (addToken (decodeUtf8 $ hash $ BS.pack token) (T.pack notes)) pool
  printf "Created Token \n, Store it safely token cannot be recovered later"

-- Check token exists in db
runAuthCommand (CheckAuthToken (CheckAuthTokenCommand token)) = do
  printf "Checking Auth token"
  pool <- fetchSqlPool
  let hashedToken = decodeUtf8 $ hash $ encodeUtf8 token
  isValidToken <- runSqlPool (checkToken hashedToken) pool
  if isValidToken
    then printf "Token is valid"
    else printf "Invalid Token"