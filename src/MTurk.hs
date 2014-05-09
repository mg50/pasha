module MTurk where
import Types
import Network.HTTP.Base (urlEncodeVars)
import Data.HMAC
import Codec.Utils
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)

url :: AWSMode -> String
url Sandbox = ""
url Production = "https://mechanicalturk.amazonaws.com/?"

askQuestion :: Config -> String -> IO ()
askQuestion config question = do
  timestamp <- getTimestamp
  let sig = getSignature config timestamp
      params = urlParams config timestamp question sig
      requestUrl = url (awsMode config) ++ urlEncodeVars params
  makeRequest requestUrl

urlParams config timestamp question signature =
  [ ("Service", service)
  , ("AWSAccessKeyId", accessKey config)
  , ("Version", "2012-03-25")
  , ("Operation", operation)
  , ("Signature", signature)
  , ("Timestamp", timestamp)
  , ("Title", "PLEASE%20HELP!!!")
  , ("Description", "HELP!!!!")
  , ("Reward.1.Amount", show (reward config))
  , ("Reward.1.CurrencyCode", "USD")
  , ("Question", formatQuestion question)
  , ("AssignmentDurationInSeconds", show $ duration config)
  , ("LifetimeInSeconds", show (lifetime config))
  ]

makeRequest = undefined
getTimestamp :: IO String
getTimestamp = do time <- getCurrentTime
                  return $ formatTime defaultTime "%Y"

getSignature :: Config -> String -> String
getSignature config timestamp = bsToString $ BS64.encode $ BS.pack sha
  where bsToString bs = map (chr . fromIntegral) $ BS.unpack bs
        sha = hmac_sha1 msg secret
        msg = strToOctets $ service ++ operation ++ show timestamp
        secret = strToOctets $ awsSecret config

strToOctets :: String -> [Octet]
strToOctets = map (fromIntegral . ord)

formatQuestion question = unlines [
    "<QuestionForm xmlns=\"" ++ questionFormSchemaURL ++ "\">"
  , "  <Overview>"
  , "  </Overview>"
  , "  <Question>"
  , "    <Text>"
  , "      " ++ question
  , "    </Text>"
  , "  </Question>"
  , "</QuestionForm>"
  ]

questionFormSchemaURL = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd"
service = "AWSMechanicalTurkRequester"
operation = "CreateHIT"
