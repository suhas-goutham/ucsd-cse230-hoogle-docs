import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (unless)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
  -- _ <- forkIO $ runTCPEchoServerForever
  -- threadDelay 1000000 -- wait one second
  sendMessage  -- "Hello, world! 1"
  threadDelay 1000000 -- wait one second

sendMessage :: IO ()
sendMessage = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock (addrAddress serveraddr)
  s <- getLine
  sendAll sock $ C.pack s
  msg <- recv sock 1024
  -- close sock
  -- delay thread to avoid client and server from printing at the same time
  -- threadDelay 1000000
  print ("TCP client received: " ++ C.unpack msg)
  if s == "quit" then
      close sock
    else sendMessage