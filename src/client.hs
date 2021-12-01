import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4040")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAndRecvMess sock

sendAndRecvMess sock = do  
  forkIO (recvMess sock)
  sendMess sock

sendMess sock = do
              s <- getLine
              case s of
                "quit" -> do
                            print ("TCP client closing")
                            close sock
                _ ->  do
                        print ("TCP client sent: " ++ s)
                        sendAll sock $ C.pack s
                        sendMess sock

recvMess sock = do
  msg <- recv sock 1024
  print ("TCP client received: " ++ C.unpack msg)
  recvMess sock
