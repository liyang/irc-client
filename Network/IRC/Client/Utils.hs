-- |
-- Module      : Network.IRC.Client.Utils
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Commonly-used utility functions for IRC clients.
module Network.IRC.Client.Utils
  ( -- * Nicks
    setNick

    -- * Channels
  , leaveChannel
  , delChan

    -- * Events
  , addHandler
  , replyTo

    -- * CTCPs
  , ctcpTo
  , ctcpReply

    -- * Connection state
  , isConnected
  , isDisconnecting
  , isDisconnected
  , snapConnState

    -- * Lenses
  , snapshot
  , snapshotModify
  ) where

import Control.Concurrent.STM (TVar, STM, atomically, modifyTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Network.IRC.Conduit (Message(..))
import Network.IRC.CTCP (toCTCP)

import Network.IRC.Client.Internal
import Network.IRC.Client.Lens

-------------------------------------------------------------------------------
-- Nicks

-- | Update the nick in the instance configuration and also send an
-- update message to the server. This doesn't attempt to resolve nick
-- collisions, that's up to the event handlers.
setNick :: Text -> IRC s ()
setNick new = do
  tvarI <- view instanceConfig <$> getIRCState
  liftIO . atomically $
    modifyTVar tvarI (set nick new)
  send $ Nick new


-------------------------------------------------------------------------------
-- Channels

-- | Update the channel list in the instance configuration and also
-- part the channel.
leaveChannel :: Text -> Maybe Text -> IRC s ()
leaveChannel chan reason = do
  tvarI <- view instanceConfig <$> getIRCState
  liftIO . atomically $ delChan tvarI chan
  send $ Part chan reason

-- | Remove a channel from the list without sending a part command (be
-- careful not to let the channel list get out of sync with the
-- real-world state if you use it for anything!)
delChan :: TVar (InstanceConfig s) -> Text -> STM ()
delChan tvarI chan = modifyTVar tvarI (over channels $ filter (/=chan))


-------------------------------------------------------------------------------
-- Events

-- | Add an event handler
addHandler :: EventHandler s -> IRC s ()
addHandler handler = do
  tvarI <- view instanceConfig <$> getIRCState
  liftIO . atomically . modifyTVar tvarI $ over handlers (handler:)

-- | Send a message to the source of an event.
replyTo :: Text{- ^ 'ChannelName' or 'NickName' -} -> Text -> IRC s ()
replyTo n = mapM_ (send . Privmsg n . Right) . T.lines


-------------------------------------------------------------------------------
-- CTCPs

-- | Construct a @PRIVMSG@ containing a CTCP
ctcpTo :: Text{- ^ 'ChannelName' or 'NickName' -} -> Text -> [Text] -> Message Text
ctcpTo n command args = Privmsg n . Left $ toCTCP command args

-- | Construct a @NOTICE@ containing a CTCP
ctcpReply :: Text -> Text -> [Text] -> Message Text
ctcpReply t command args = Notice t . Left $ toCTCP command args


-------------------------------------------------------------------------------
-- Connection state

-- | Check if the client is connected.
isConnected :: IRC s Bool
isConnected = (==Connected) <$> snapConnState

-- | Check if the client is in the process of disconnecting.
isDisconnecting :: IRC s Bool
isDisconnecting = (==Disconnecting) <$> snapConnState

-- | Check if the client is disconnected
isDisconnected :: IRC s Bool
isDisconnected = (==Disconnected) <$> snapConnState

-- | Snapshot the connection state.
snapConnState :: IRC s ConnectionState
snapConnState = liftIO . atomically . getConnectionState =<< getIRCState
