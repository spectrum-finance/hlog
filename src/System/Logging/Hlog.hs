module System.Logging.Hlog
  ( Loggable(..)
  , Logging(..)
  , MakeLogging(..)
  , LoggingConfig(..)
  , LogLevel(..)
  , makeLogging
  , translateMakeLogging
  ) where

import RIO   hiding (LogLevel)
import Dhall        (FromDhall)

import System.Log.Logger as SL
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

data LogLevel
  = Info
  | Error
  | Warn
  | Debug
  deriving (Generic, Eq, Show, FromDhall)

type Format = String

type Component = String

data LoggingConfig = LoggingConfig
  { fileHandlers   :: ![(FilePath, LogLevel, Format)]
  , levelOverrides :: ![(Component, LogLevel)]
  , rootLogLevel   :: !LogLevel
  } deriving (Generic, Show, FromDhall)

instance FromDhall (FilePath, LogLevel, Format)

class Loggable a where
  toLog :: a -> String

instance Loggable String where
  toLog = id

data Logging f = Logging
  { debugM :: forall a. Loggable a => a -> f ()
  , infoM  :: forall a. Loggable a => a -> f ()
  , warnM  :: forall a. Loggable a => a -> f ()
  , errorM :: forall a. Loggable a => a -> f ()
  }

loggingForComponent :: (Applicative f, MonadIO m) => Component -> f (Logging m)
loggingForComponent name = do
  pure Logging
    { debugM = (liftIO . SL.debugM name) . toLog
    , infoM  = (liftIO . SL.infoM name) . toLog
    , warnM  = (liftIO . SL.warningM name) . toLog
    , errorM = (liftIO . SL.errorM name) . toLog
    }

newtype MakeLogging f m = MakeLogging
  { forComponent :: Component -> f (Logging m)
  }

translateMakeLogging :: (forall a. f a -> g a) -> MakeLogging f m -> MakeLogging g m
translateMakeLogging funK MakeLogging{..} =
  MakeLogging $ funK . forComponent

makeLogging :: (MonadIO f, MonadIO m) => LoggingConfig -> f (MakeLogging f m)
makeLogging LoggingConfig{..} = do
  let
    setHandler (path, level, format) = do
      lh <- fileHandler path (fromHlogLevel level) <&> (flip setFormatter) (simpleLogFormatter format)
      updateGlobalLogger rootLoggerName (addHandler lh)
    overrideLevel (component, level) = updateGlobalLogger component (setLevel $ fromHlogLevel level)
  liftIO $ updateGlobalLogger rootLoggerName (setLevel (fromHlogLevel rootLogLevel))
  RIO.void . liftIO $ mapM setHandler fileHandlers
  RIO.void . liftIO $ mapM overrideLevel levelOverrides
  RIO.void . liftIO $ print "Init logging!"
  pure MakeLogging
    { forComponent = loggingForComponent
    }

fromHlogLevel :: LogLevel -> Priority
fromHlogLevel Debug = DEBUG
fromHlogLevel Info  = INFO
fromHlogLevel Warn  = WARNING
fromHlogLevel _     = ERROR
