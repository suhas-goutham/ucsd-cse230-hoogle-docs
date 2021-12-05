import Control.Concurrent       
import Control.Monad             
-- import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket 
import Network.Socket.ByteString (recv, sendAll)
import System.IO()
import Control.Monad.Fix (fix)

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4444")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 2
  chan <- newChan
  runTCPEchoServerForever sock chan 0

runTCPEchoServerForever :: (Eq a, Num a) => Socket -> Chan (a, String) -> a -> IO b
runTCPEchoServerForever sock chan msgNum = do 
  (conn, _) <- accept sock
  forkIO (rrLoop conn chan msgNum) -- conn and sock are same
  runTCPEchoServerForever sock chan $! msgNum + 1


rrLoop sock chan msgNum = do
  let broadcast msg = writeChan chan (msgNum, msg)
  
  broadcast "--> new person entered chat"
  
  commLine <- dupChan chan

  reader <- forkIO $ fix $ \loop -> do
        (nextNum, mes) <- readChan commLine
        when (msgNum /= nextNum) $ sendAll sock (C.pack mes)
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