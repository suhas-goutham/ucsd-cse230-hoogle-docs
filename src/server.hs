import Control.Concurrent       
import Control.Monad             
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4040")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 2
  chan <- newChan
  runTCPEchoServerForever sock chan

runTCPEchoServerForever sock chan = do 
  (conn, _) <- accept sock
  forkIO (rrLoop conn chan) -- conn and sock are same
  runTCPEchoServerForever sock chan

type Msg = String

rrLoop sock chan = do
  let broadcast msg = writeChan chan msg
  
  broadcast "--> new person entered chat"
  
  commLine <- dupChan chan

  reader <- forkIO $ fix $ \loop -> do
        mes <- readChan commLine
        sendAll sock (C.pack mes)
        loop

  writer sock broadcast
  killThread reader
writer sock broadcast = do
                        msg <- recv sock 1024
                        let s = C.unpack msg
                        case s of
                          "quit" -> do
                                      print "TCP client closing"
                                      threadDelay 100000
                                      close sock
                          _      -> do
                                      broadcast s
                                      print ("TCP server received: " ++ s)
                                      threadDelay 100000
                                      writer sock broadcast
