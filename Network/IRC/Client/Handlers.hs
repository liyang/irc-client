{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.IRC.Client.Handlers
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- The default event handlers. Handlers are invoked concurrently when
-- matching events are received from the server.
module Network.IRC.Client.Handlers
  ( -- * Event handlers
    defaultEventHandlers
  , pingHandler
  , ctcpPingHandler
  , ctcpVersionHandler
  , ctcpTimeHandler
  , welcomeNick
  , joinOnWelcome
  , nickMangler

  -- * Special handlers
  , defaultOnConnect
  , defaultOnDisconnect
  ) where

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (atomically, readTVar, modifyTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Char              (isAlphaNum)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Text              (Text, breakOn, takeEnd)
import Data.Time.Clock        (getCurrentTime)
import Data.Time.Format       (formatTime)
import Network.IRC.CTCP       (fromCTCP)

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale    (defaultTimeLocale)
#endif

import qualified Data.Text as T

import Network.IRC.Client.Types
import Network.IRC.Client.Utils
import Network.IRC.Client.Internal

-- * Event handlers

-- | The default event handlers, the following are included:
--
-- - respond to server @PING@ messages with a @PONG@;
-- - respond to CTCP @PING@ requests with a CTCP @PONG@;
-- - respond to CTCP @VERSION@ requests with the version string;
-- - respond to CTCP @TIME@ requests with the system time;
-- - update the nick upon receiving the welcome message, in case the
--   server modifies it;
-- - mangle the nick if the server reports a collision;
-- - update the channel list on @JOIN@ and @KICK@.
--
-- These event handlers are all exposed through the
-- Network.IRC.Client.Handlers module, so you can use them directly if
-- you are building up your 'InstanceConfig' from scratch.
--
-- If you are building a bot, you may want to write an event handler
-- to process messages representing commands.
defaultEventHandlers :: [EventHandler s]
defaultEventHandlers =
  [ eventHandler (matchType EPing) pingHandler
  , eventHandler (matchType EKick) kickHandler
  , eventHandler (matchCTCP ["PING"]) ctcpPingHandler
  , eventHandler (matchCTCP ["TIME"]) ctcpTimeHandler
  , eventHandler (matchCTCP ["VERSION"]) ctcpVersionHandler
  , eventHandler (matchNumeric [001]) welcomeNick
  , eventHandler (matchNumeric [001]) joinOnWelcome
  , eventHandler (matchNumeric [332]) joinHandler
  , eventHandler (matchNumeric [432, 433, 436]) nickMangler
  ]

-- | Respond to server @PING@ messages with a @PONG@.
pingHandler :: Event Text -> StatefulIRC s ()
pingHandler ev = case _message ev of
  Ping s1 s2 -> send . Pong $ fromMaybe s1 s2
  _ -> return ()

-- | Respond to CTCP @PING@ requests with a CTCP @PONG@.
ctcpPingHandler :: Event Text -> StatefulIRC s ()
ctcpPingHandler ev = case (_source ev, _message ev) of
  (User n, Privmsg _ (Left ctcpbs)) ->
    let (_, xs) = fromCTCP ctcpbs
    in send $ ctcpReply n "PING" xs
  _ -> pure ()

-- | Respond to CTCP @VERSION@ requests with the version string.
ctcpVersionHandler :: Event Text -> StatefulIRC s ()
ctcpVersionHandler ev = case _source ev of
  User n -> do
    ver <- get version <$> (snapshot instanceConfig =<< getIrcState)
    send $ ctcpReply n "VERSION" [ver]
  _ -> pure ()

-- | Respond to CTCP @TIME@ requests with the system time.
ctcpTimeHandler :: Event Text -> StatefulIRC s ()
ctcpTimeHandler ev = case _source ev of
  User n -> do
    now <- liftIO getCurrentTime
    send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]
  _ -> pure ()

-- | Update the nick upon welcome (numeric reply 001), as it may not
-- be what we requested (eg, in the case of a nick too long).
welcomeNick :: Event Text -> StatefulIRC s ()
welcomeNick ev = case _message ev of
    Numeric _ xs ->  go xs
    _ -> pure ()
  where
    go (srvNick:_) = do
      tvarI <- get instanceConfig <$> getIrcState
      liftIO . atomically $
        modifyTVar tvarI (set nick srvNick)
    go _ = pure ()

-- | Join default channels upon welcome (numeric reply 001). If sent earlier,
-- the server might reject the JOIN attempts.
joinOnWelcome :: Event Text -> StatefulIRC s ()
joinOnWelcome _ = do
  iconf <- snapshot instanceConfig =<< getIrcState
  mapM_ (send . Join) $ get channels iconf

-- | Mangle the nick if there's a collision (numeric replies 432, 433,
-- and 436) when we set it
nickMangler :: Event Text -> StatefulIRC s ()
nickMangler ev = case _message ev of
    Numeric 432 xs -> go fresh xs
    Numeric 433 xs -> go mangle xs
    Numeric 436 xs -> go mangle xs
    _ -> pure ()
  where
    go f (_:srvNick:_) = do
      theNick <- get nick <$> (snapshot instanceConfig =<< getIrcState)

      -- If the length of our nick and the server's idea of our nick
      -- differ, it was truncated - so calculate the allowable length.
      let nicklen = if T.length srvNick /= T.length theNick
                    then Just $ T.length srvNick
                    else Nothing

      setNick . trunc nicklen $ f srvNick
    go _ _ = return ()

    fresh n = if T.length n' == 0 then "f" else n'
      where n' = T.filter isAlphaNum n

    mangle n = (n <> "1") `fromMaybe` charsubst n

    -- Truncate a nick, if there is a known length limit.
    trunc len txt = maybe txt (`takeEnd` txt) len

    -- List of substring substitutions. It's important that these
    -- don't contain any loops!
    charsubst = transform [ ("i", "1")
                          , ("I", "1")
                          , ("l", "1")
                          , ("L", "1")
                          , ("o", "0")
                          , ("O", "0")
                          , ("A", "4")
                          , ("0", "1")
                          , ("1", "2")
                          , ("2", "3")
                          , ("3", "4")
                          , ("4", "5")
                          , ("5", "6")
                          , ("6", "7")
                          , ("7", "8")
                          , ("8", "9")
                          , ("9", "-")
                          ]

    -- Attempt to transform some text by the substitutions.
    transform ((from, to):trs) txt = case breakOn' from txt of
      Just (before, after) -> Just $ before <> to <> after
      _ -> transform trs txt
    transform [] _ = Nothing

-- | Upon receiving a channel topic (numeric reply 332), add the
-- channel to the list (if not already present).
joinHandler :: Event Text -> StatefulIRC s ()
joinHandler ev = case _message ev of
    Numeric _ xs -> go xs
    _ -> pure ()
  where
    go (c:_) = do
      tvarI <- get instanceConfig <$> getIrcState
      liftIO . atomically $
        modifyTVar tvarI $ \iconf ->
          (if c `elem` get channels iconf
            then modify channels (c:)
            else id) iconf

    go _ = pure ()

-- | Update the channel list upon being kicked.
kickHandler :: Event Text -> StatefulIRC s ()
kickHandler ev = do
  tvarI <- get instanceConfig <$> getIrcState
  liftIO . atomically $ do
    theNick <- get nick <$> readTVar tvarI
    case (_source ev, _message ev) of
      (Channel c _, Kick n _ _) | n == theNick -> delChan tvarI c
                                | otherwise    -> pure ()
      _ -> pure ()

-- *Special

-- | The default connect handler: set the nick.
defaultOnConnect :: StatefulIRC s ()
defaultOnConnect = do
  iconf <- snapshot instanceConfig =<< getIrcState
  send . Nick $ get nick iconf

-- | The default disconnect handler: do nothing. You might want to
-- override this with one which reconnects.
defaultOnDisconnect :: StatefulIRC s ()
defaultOnDisconnect = return ()

-- *Utils

-- | Break some text on the first occurrence of a substring, removing
-- the substring from the second portion.
breakOn' :: Text -> Text -> Maybe (Text, Text)
breakOn' delim txt = if T.length after >= T.length delim
                     then Just (before, T.drop (T.length delim) after)
                     else Nothing
  where
    (before, after) = breakOn delim txt
