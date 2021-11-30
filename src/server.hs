import Control.Concurrent       
import Control.Monad             
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import System.IO()

main :: IO ()
main = do
  -- _ <- forkIO $ runTCPEchoServerForever
  chan <- newChan
  runTCPEchoServerForever chan
  threadDelay 1000000 -- wait one second

--runTCPEchoServerForever :: Chan -> IO ()
runTCPEchoServerForever chan = do 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 2
  -- (conn, _) <- accept sock
  rrLoop sock chan
  print "TCP server socket is closing now."
  -- close sock
  
  where
    rrLoop sock chan = do
      (conn, _) <- accept sock
      forkIO (runConn conn chan)
      rrLoop sock chan
    
runConn conn chan = do
        msg <- recv conn 1024 
        unless (BS.null msg) $ do 
          writeChan chan "From SERVER: Hello!\n"
          print ("TCP server received: " ++ C.unpack msg)
          print "TCP server is now sending a message to the client"
          sendAll conn msg
        runConn conn chan