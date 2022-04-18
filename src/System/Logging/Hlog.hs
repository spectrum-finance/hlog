module System.Logging.Hlog
  ( Loggable(..)
  , Logging(..)
  , MakeLogging(..)
  , LoggingConfig(..)
  , LogLevel(..)
  , makeLogging
  ) where

import RIO hiding (LogLevel)

import System.Log.Logger as SL
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

data LogLevel
  = Info
  | Error
  | Warn
  | Debug

type Format = String

type Component = String

data LoggingConfig = LoggingConfig
  { fileHandlers   :: [(FilePath, LogLevel, Format)]
  , levelOverrides :: [(Component, LogLevel)]
  }

class Loggable a where
  toLog :: a -> String

data Logging f = Logging
  { debugM :: forall a. Loggable a => a -> f ()
  , infoM  :: forall a. Loggable a => a -> f ()
  , warnM  :: forall a. Loggable a => a -> f ()
  , errorM :: forall a. Loggable a => a -> f ()
  }

loggingForComponent :: MonadIO f => Component -> Logging f
loggingForComponent name = do
  Logging
    { debugM = (liftIO . SL.debugM name) . toLog
    , infoM  = (liftIO . SL.infoM name) . toLog
    , warnM  = (liftIO . SL.warningM name) . toLog
    , errorM = (liftIO . SL.errorM name) . toLog
    }

data MakeLogging f = MakeLogging
  { forComponent :: Component -> Logging f
  }

makeLogging :: (MonadIO f, MonadIO m) => LoggingConfig -> f (MakeLogging m)
makeLogging LoggingConfig{..} = do
  let
    setHandler (path, level, format) = do
      lh <- fileHandler path (fromHlogLevel level) <&> (flip setFormatter) (simpleLogFormatter format)
      updateGlobalLogger rootLoggerName (addHandler lh)
    overrideLevel (component, level) = updateGlobalLogger component (setLevel $ fromHlogLevel level)
  liftIO $ updateGlobalLogger rootLoggerName (setLevel DEBUG)
  _ <- liftIO $ mapM setHandler fileHandlers
  _ <- liftIO $ mapM overrideLevel levelOverrides
  pure MakeLogging
    { forComponent = loggingForComponent
    }

fromHlogLevel :: LogLevel -> Priority
fromHlogLevel Debug = DEBUG
fromHlogLevel Info  = INFO
fromHlogLevel Warn  = WARNING
fromHlogLevel _     = ERROR
