import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://localhost:3000/"
  response <- httpLbs request manager

  case statusCode $ responseStatus response of
    200 -> print $ responseBody response
    _   -> putStrLn "ERROR: Well, this wasn't the statuscode I was expecting."

