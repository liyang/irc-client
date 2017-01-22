{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Events
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings, RankNTypes
--
-- Events and event handlers. When a message is received from the
-- server, all matching handlers are executed concurrently.
module Network.IRC.Client.Events
  ( -- * Handlers
    EventHandler(..)
  , match
  , ctcp
  , numeric
{-   , matchWhen -}

  -- * Default handlers
  , defaultEventHandlers
  , defaultOnConnect
  , defaultOnDisconnect

  -- ** Individual handlers
  , pingHandler
  , ctcpPingHandler
  , ctcpVersionHandler
  , ctcpTimeHandler
  , welcomeNick
  , joinOnWelcome
  , joinHandler
  , nickMangler

  -- * Re-exported
  , Event(..)
  , Message(..)
  , Source(..)
  , module Network.IRC.Conduit.Lens
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (atomically, readTVar, modifyTVar)
import Control.Monad
import Control.Monad.Catch (SomeException, fromException, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Contravariant (phantom)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, breakOn, takeEnd, toUpper)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.IRC.CTCP (fromCTCP)
import Network.IRC.Conduit
import Network.IRC.Conduit.Lens

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import qualified Data.Text as T

import Network.IRC.Client.Internal
import Network.IRC.Client.Lens
import Network.IRC.Client.Utils


-------------------------------------------------------------------------------
-- Handlers

-- | Match the verb of a CTCP, ignoring case, and returning the arguments.
--
-- > ctcp "ping"   ":foo PRIVMSG #bar :\001PING\001"          ==> Just []
-- > ctcp "PING"   ":foo PRIVMSG #bar :\001PING\001"          ==> Just []
-- > ctcp "ACTION" ":foo PRIVMSG #bar :\001ACTION dances\001" ==> Just ["dances"]
ctcp :: Text -> Fold (Message a) [Text]
ctcp verb = _Privmsg . to snd . _Left . to fromCTCP
    . filtered (on (==) toUpper verb . fst) . to snd

-- | Match a numeric server message. Numeric messages are sent in
-- response to most things, such as connecting to the server, or
-- joining a channel.
--
-- Numerics in the range 001 to 099 are informative messages, numerics
-- in the range 200 to 399 are responses to commands. Some common
-- numerics are:
--
--    - 001 (RPL_WELCOME), sent after successfully connecting.
--
--    - 331 (RPL_NOTOPIC), sent after joining a channel if it has no
--      topic.
--
--    - 332 (RPL_TOPIC), sent after joining a channel if it has a
--      topic.
--
--    - 432 (ERR_ERRONEUSNICKNAME), sent after trying to change to an
--      invalid nick.
--
--    - 433 (ERR_NICKNAMEINUSE), sent after trying to change to a nick
--      already in use.
--
--    - 436 (ERR_NICKCOLLISION), sent after trying to change to a nick
--      in use on another server.
--
-- See Section 5 of @<https://tools.ietf.org/html/rfc2812#section-5 RFC 2812>@
-- for a complete list.
--
-- > Numeric 001 xs ^? numeric (001 ==) ==> Just xs
-- > Numeric numeric (332 ==) "332 :#haskell: We like Haskell"  ==> True
numeric :: (Int -> Bool) -> Fold (Message a) [a]
numeric p = _Numeric . filtered (p . fst) . to snd

-- | Helper to match on the event 'Source' and 'Message' type. See
-- "Network.IRC.Conduit.Lens#Source" and "Network.IRC.Conduit.Lens#Message"
-- for the optics that can be used here.
--
-- FIXME: examples
-- > matchType _Privmsg ":foo PRIVMSG #bar :hello world" ==> Just "hello world"
-- > matchType _Quit    ":foo QUIT :goodbye world"       ==> Just "goodbye world"
match :: Fold (Source a) x -> Fold (Message a) y -> Fold (Event a) (x, y)
match src msg = pair (source . src) (message . msg) where
    folding :: Foldable f => (s -> f a) -> Fold s a
    folding sfa agb = phantom . traverse_ agb . sfa
    pair :: Fold s a -> Fold s b -> Fold s (a, b)
    pair l r = folding $ \ s -> (,) <$> preview l s <*> preview r s
-- with lens
{- match src msg = runFold $ (,) <$> Fold (source . src) <*> Fold (message . msg) -}

-- | Match a predicate against an event.
--
-- > matchWhen (const True) ":foo PRIVMSG #bar :hello world" ==> Just "PRIVMSG :hello world"
{- matchWhen :: (Event a -> Bool) -> Event a -> Maybe (Message a) -}
{- matchWhen p ev | p ev = Just (_message ev) -}
{- matchWhen _ _ = Nothing -}


-------------------------------------------------------------------------------
-- Default handlers

-- | The default event handlers, the following are included:
--
-- - respond to server @PING@ messages with a @PONG@;
-- - respond to CTCP @PING@ requests;
-- - respond to CTCP @VERSION@ requests with the version string;
-- - respond to CTCP @TIME@ requests with the system time;
-- - update the nick upon receiving the welcome message, in case the
--   server modifies it;
-- - mangle the nick if the server reports a collision;
-- - update the channel list on @JOIN@ and @KICK@.
defaultEventHandlers :: [EventHandler s]
defaultEventHandlers =
  [ pingHandler
  , kickHandler
  , ctcpPingHandler
  , ctcpTimeHandler
  , ctcpVersionHandler
  , welcomeNick
  , joinOnWelcome
  , joinHandler
  , nickMangler
  ]

-- | The default connect handler: set the nick.
defaultOnConnect :: IRC s ()
defaultOnConnect = do
  iconf <- snapshot instanceConfig
  send . Nick $ view nick iconf

-- | The default disconnect handler
--
--    - If the client disconnected due to a 'Timeout' exception, reconnect.
--
--    - If the client disconnected due to another exception, rethrow it.
--
--    - If the client disconnected without an exception, halt.
defaultOnDisconnect :: Maybe SomeException -> IRC s ()
defaultOnDisconnect (Just exc) = case fromException exc of
  Just Timeout -> reconnect
  Nothing -> throwM exc
defaultOnDisconnect Nothing = pure ()


-------------------------------------------------------------------------------
-- Individual handlers

-- | Respond to server @PING@ messages with a @PONG@.
pingHandler :: EventHandler s
pingHandler = EventHandler (match _Server _Ping) $
    \ (_server, (s, ms)) -> send (Pong $ fromMaybe s ms)

-- | Respond to CTCP @PING@ requests.
ctcpPingHandler :: EventHandler s
ctcpPingHandler = EventHandler (match _User $ ctcp "PING") $
    \ (n, xs) -> send (ctcpReply n "PING" xs)

-- | Respond to CTCP @VERSION@ requests with the version string.
ctcpVersionHandler :: EventHandler s
ctcpVersionHandler = EventHandler (match _User $ ctcp "VERSION") $ \ (n, _xs) -> do
    ver <- view version <$> snapshot instanceConfig
    send $ ctcpReply n "VERSION" [ver]

-- | Respond to CTCP @TIME@ requests with the system time.
ctcpTimeHandler :: EventHandler s
ctcpTimeHandler = EventHandler (match _User $ ctcp "TIME") $ \ (n, _xs) -> do
    now <- liftIO getCurrentTime
    send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]

-- | Update the nick upon welcome (numeric reply 001), as it may not
-- be what we requested (eg, in the case of a nick too long).
welcomeNick :: EventHandler s
-- FIXME: _Cons
welcomeNick = EventHandler (match id $ numeric (001 ==)) $ \ (_, xs) -> case xs of
    (srvNick : _) -> do
        tvarI <- view instanceConfig <$> getIRCState
        liftIO . atomically $
            modifyTVar tvarI (set nick srvNick)
    _ -> pure ()

-- | Join default channels upon welcome (numeric reply 001). If sent earlier,
-- the server might reject the JOIN attempts.
joinOnWelcome :: EventHandler s
joinOnWelcome = EventHandler (message . numeric (001 ==)) $ \ _xs -> do
  iconf <- snapshot instanceConfig
  mapM_ (send . Join) $ view channels iconf

-- | Mangle the nick if there's a collision (numeric replies 432, 433,
-- and 436) when we set it
nickMangler :: EventHandler s
nickMangler = EventHandler matcher go where
    matcher :: Fold (Event Text) (ServerName Text, (Text -> Text, [Text]))
    matcher = match _Server $ _Numeric . filtered
        (flip elem [432, 433, 436] . fst) . to (over _1 how)
    how n = if n == 432 then fresh else mangle

    go (_server, (f, _:srvNick:_)) = do
      theNick <- view nick <$> (snapshot instanceConfig =<< getIRCState)

      -- If the length of our nick and the server's idea of our nick
      -- differ, it was truncated - so calculate the allowable length.
      let nicklen = if T.length srvNick /= T.length theNick
                    then Just $ T.length srvNick
                    else Nothing

      setNick . trunc nicklen $ f srvNick
    go _ = return ()

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
    transform ((src, dst):trs) txt = case breakOn' src txt of
      Just (before, after) -> Just $ before <> dst <> after
      _ -> transform trs txt
    transform [] _ = Nothing

-- | Upon joining a channel (numeric reply 331 or 332), add it to the
-- list (if not already present).
joinHandler :: EventHandler s
joinHandler = EventHandler matcher go where
    matcher :: Fold (Event Text) (Source Text, [Text])
    matcher = match id{-FIXME-} $ _Numeric
        . filtered (flip elem [331, 332] . fst) . to snd
    go (_src, args) = case args of
        (c:_) -> do
            tvarI <- view instanceConfig <$> getIRCState
            liftIO . atomically $ modifyTVar tvarI $ over channels $
                \ cs -> if c `elem` cs then cs else c : cs
        _ -> pure ()

-- | Update the channel list upon being kicked.
kickHandler :: EventHandler s
kickHandler = EventHandler (match _Channel _Kick) $ \ ((c, _), (n, _, _)) -> do
  tvarI <- view instanceConfig <$> getIRCState
  liftIO . atomically $ do
    theNick <- view nick <$> readTVar tvarI
    when (n == theNick) $ do
      delChan tvarI c


-------------------------------------------------------------------------------
-- Utils

-- | Break some text on the first occurrence of a substring, removing
-- the substring from the second portion.
breakOn' :: Text -> Text -> Maybe (Text, Text)
breakOn' delim txt = if T.length after >= T.length delim
                     then Just (before, T.drop (T.length delim) after)
                     else Nothing
  where
    (before, after) = breakOn delim txt
