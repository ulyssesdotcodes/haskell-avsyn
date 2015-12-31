{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

import System.IO
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Text (Text)
import Data.Unique
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static
import qualified Sound.OSC as OSC
import AesonOSC
import Cinder

type MessageQueue = Chan OSC.Message

-- Server state
type Client = (Unique, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

-- Socket messaging

broadcast :: ByteString -> ServerState -> IO ()
broadcast message clients = forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcastBundle:: OSC.Bundle -> ServerState -> IO ()
broadcastBundle bundle = broadcast $ bundleToJSON bundle

bundleToJSON :: OSC.Bundle -> ByteString
bundleToJSON = A.encode . A.toJSON

extractMessages :: ByteString -> [OSC.Message]
extractMessages bs =  messagesFromMaybe $ fmap bundleToMessages $ join . either (const Nothing) Just $ A.eitherDecode bs

messagesFromMaybe :: Maybe [OSC.Message] -> [OSC.Message]
messagesFromMaybe Nothing = []
messagesFromMaybe (Just x) = x

modifyMixer :: MVar Mixer -> OSC.Message -> IO ()
modifyMixer state message = modifyMVar_ state $ \mixer -> do
    let mixerState = applyMessage message mixer
    sendUDPMessages (snd mixerState)
    return $ fst mixerState

receiveSocketMessages :: MVar Mixer -> WS.Connection -> IO ()
receiveSocketMessages state conn = forever $ do
  (msg :: ByteString) <- WS.receiveData conn
  let messages = extractMessages msg
  mapM_ (modifyMixer state) messages

bundleToMessages :: OSC.Bundle -> [OSC.Message]
bundleToMessages = OSC.bundleMessages

messagesToBundle :: [OSC.Message] -> OSC.Bundle
messagesToBundle = OSC.bundle OSC.immediately

-- OSC messaging
sendUDPMessage :: OSC.Message -> IO ()
sendUDPMessage  message = do
  print message
  hFlush stdout
  OSC.withTransport (OSC.openUDP "127.0.0.1" 3334) $ OSC.sendMessage message

sendUDPMessages :: [ OSC.Message ] -> IO ()
sendUDPMessages messages = do
  print messages
  hFlush stdout
  OSC.withTransport (OSC.openUDP "127.0.0.1" 3334) $ OSC.sendBundle (OSC.Bundle 0.0 messages)

receiveMessageTransport :: IO OSC.UDP -> MessageQueue -> IO ()
receiveMessageTransport t mChan = OSC.withTransport t $ forever $ do
  msg <- OSC.waitMessage
  liftIO $ writeChan mChan msg

receiveMessages :: MessageQueue -> IO ()
receiveMessages = receiveMessageTransport $ OSC.udpServer "0.0.0.0" 3333

handleUDPMessages:: MVar ServerState -> MessageQueue -> IO ()
handleUDPMessages state mChan = do
  msg <- readChan mChan
  clients <- liftIO $ readMVar state
  handleUDPMessages state mChan

-- Serve webpage and init sockets
server :: MessageQueue -> IO()
server mq = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  serverState <- newMVar newServerState
  cinderState <- newMVar newCinderState
  __ <- forkIO $ handleUDPMessages serverState mq
  sendUDPMessage $ OSC.message "/connection" []
  Warp.run
    port
    $ WaiWS.websocketsOr WS.defaultConnectionOptions (application cinderState serverState) staticApp

staticApp :: Network.Wai.Application
staticApp = Static.staticApp $ Static.defaultWebAppSettings "C:\\Users\\Ulysses\\Development\\avsyn-web-interface\\public"

application :: MVar Mixer -> MVar ServerState -> WS.ServerApp
application cinderState serverState pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  id <- newUnique
  let client = (id, conn)
  flip finally (disconnect client) $ do
    liftIO $ modifyMVar_ serverState $ \s -> do
        let s' = addClient client s
        print "Added client"
        hFlush stdout
        return s'
    mixer <- readMVar cinderState
    (WS.sendTextData conn) . bundleToJSON . messagesToBundle $ mixerToMessages mixer
    receiveSocketMessages cinderState conn
    where
        disconnect client = do
            s <- modifyMVar_ serverState $ \s ->
                let s' = removeClient client s in return s'
            print "Removed client"

main :: IO ()
main = do
  m <- newChan
  forkIO $ receiveMessages m
  server m