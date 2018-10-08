{-# LANGUAGE CPP #-}

module XMonad.Util.ALSA
    ( toggleMute
    , raiseVolume
    , lowerVolume
    , getVolume
    , getMute
    , getVolumeMute
    , setVolume
    , setMute
    , setVolumeMute
    , modifyVolume
    , modifyMute
    , modifyVolumeMute
    , defaultChannels
    , toggleMuteChannels
    , raiseVolumeChannels
    , lowerVolumeChannels
    , getVolumeChannels
    , getMuteChannels
    , getVolumeMuteChannels
    , setVolumeChannels
    , setMuteChannels
    , setVolumeMuteChannels
    , modifyVolumeChannels
    , modifyMuteChannels
    , modifyVolumeMuteChannels
    ) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe
import           Sound.ALSA.Mixer

toggleMute          :: MonadIO m => m Bool
raiseVolume         :: MonadIO m => Double -> m Double
lowerVolume         :: MonadIO m => Double -> m Double
getVolume           :: MonadIO m => m Double
getMute             :: MonadIO m => m Bool
getVolumeMute       :: MonadIO m => m (Double, Bool)
setVolume           :: MonadIO m => Double         -> m ()
setMute             :: MonadIO m => Bool           -> m ()
setVolumeMute       :: MonadIO m => Double -> Bool -> m ()
modifyVolume        :: MonadIO m => (Double         -> Double)         -> m Double
modifyMute          :: MonadIO m => (Bool           -> Bool)           -> m Bool
modifyVolumeMute    :: MonadIO m => (Double -> Bool -> (Double, Bool)) -> m (Double, Bool)

toggleMute          = toggleMuteChannels       defaultChannels
raiseVolume         = raiseVolumeChannels      defaultChannels
lowerVolume         = lowerVolumeChannels      defaultChannels
getVolume           = getVolumeChannels        defaultChannels
getMute             = getMuteChannels          defaultChannels
getVolumeMute       = getVolumeMuteChannels    defaultChannels
setVolume           = setVolumeChannels        defaultChannels
setMute             = setMuteChannels          defaultChannels
setVolumeMute       = setVolumeMuteChannels    defaultChannels
modifyVolume        = modifyVolumeChannels     defaultChannels
modifyMute          = modifyMuteChannels       defaultChannels
modifyVolumeMute    = modifyVolumeMuteChannels defaultChannels

defaultChannels :: [String]
defaultChannels = ["Master", "Wave", "PCM"]

toggleMuteChannels          :: MonadIO m => [String] -> m Bool
raiseVolumeChannels         :: MonadIO m => [String] -> Double -> m Double
lowerVolumeChannels         :: MonadIO m => [String] -> Double -> m Double
getVolumeChannels           :: MonadIO m => [String] -> m Double
getMuteChannels             :: MonadIO m => [String] -> m Bool
getVolumeMuteChannels       :: MonadIO m => [String] -> m (Double, Bool)
setVolumeChannels           :: MonadIO m => [String] -> Double         -> m ()
setMuteChannels             :: MonadIO m => [String] -> Bool           -> m ()
setVolumeMuteChannels       :: MonadIO m => [String] -> Double -> Bool -> m ()
modifyVolumeChannels        :: MonadIO m => [String] -> (Double         -> Double )        -> m Double
modifyMuteChannels          :: MonadIO m => [String] -> (Bool           -> Bool )          -> m Bool
modifyVolumeMuteChannels    :: MonadIO m => [String] -> (Double -> Bool -> (Double, Bool)) -> m (Double, Bool)

toggleMuteChannels  cs = modifyMuteChannels   cs not
raiseVolumeChannels cs = modifyVolumeChannels cs . (+)
lowerVolumeChannels cs = modifyVolumeChannels cs . (subtract)

getVolumeChannels     = liftIO . fmap fst . alsaGetAll
getMuteChannels       = liftIO . fmap snd . alsaGetAll
getVolumeMuteChannels = liftIO            . alsaGetAll

setVolumeChannels     cs v   = liftIO (alsaSetVolumeAll v   cs)
setMuteChannels       cs   m = liftIO (alsaSetMuteAll     m cs)
setVolumeMuteChannels cs v m = liftIO (alsaSetAll       v m cs)

modifyVolumeChannels = modify getVolumeChannels setVolumeChannels
modifyMuteChannels   = modify getMuteChannels   setMuteChannels
modifyVolumeMuteChannels cs = modify getVolumeMuteChannels (\cs' -> uncurry (setVolumeMuteChannels cs')) cs . uncurry

geomMean :: Floating a => [a] -> a
geomMean xs = product xs ** (recip . fromIntegral . length $ xs)

clip :: (Num t, Ord t) => t -> t
clip = min 100 . max 0

toRange :: (Integer, Integer) -> Double -> Integer
toRange (x, y) d = floor (d * (y' - x') / 100 + x')
    where x' = fromIntegral x
          y' = fromIntegral y

fromRange :: (Integer, Integer) -> Integer -> Double
fromRange (x, y) z = fromIntegral (z - x) / fromIntegral (y - x) * 100

modify :: Monad m => (arg -> m value) -> (arg -> value -> m ()) -> arg -> (value -> value) -> m value
modify get set cs f = do
    v <- liftM f $ get cs
    set cs v
    return v

withControl :: (Control -> IO a) -> [String] -> IO a
withControl f cs = withMixer "default" $ \mixer -> do
    (control:_) <- catMaybes <$> mapM (getControlByName mixer) cs
    f control

alsaGetAll :: [String] -> IO (Double, Bool)
alsaGetAll = withControl $ \control -> (,)
    <$> alsaGetVolume control
    <*> alsaGetMute control

alsaGetVolume :: Control -> IO Double
alsaGetVolume control = do
    let Just playbackVolume = playback $ volume control
        volChans = value playbackVolume
    range <- getRange playbackVolume
    vals <- mapM (\chan -> getChannel chan volChans) (channels volChans)
    return $ geomMean $ map (fromRange range . fromJust) vals

alsaGetMute :: Control -> IO Bool
alsaGetMute control = do
    let Just muteChans = playback $ switch control
    all id . map fromJust <$> mapM (\chan -> getChannel chan muteChans) (channels muteChans)

alsaSetVolumeAll :: Double -> [String] -> IO ()
alsaSetVolumeAll v = withControl (alsaSetVolume v)

alsaSetVolume :: Double -> Control -> IO ()
alsaSetVolume v control = do
    let Just playbackVolume = playback $ volume control
        volChans = value playbackVolume
    range <- getRange playbackVolume
    forM_ (channels volChans) $ \chan -> do
        setChannel chan volChans (toRange range (clip v))

alsaSetMuteAll :: Bool -> [String] -> IO ()
alsaSetMuteAll m = withControl (alsaSetMute m)

alsaSetMute :: Bool -> Control -> IO ()
alsaSetMute m control = do
    let Just muteChans = playback $ switch control
    forM_ (channels muteChans) $ \chan -> setChannel chan muteChans m

alsaSetAll :: Double -> Bool -> [String] -> IO ()
alsaSetAll v m = withControl $ \control -> do
    alsaSetVolume v control
    alsaSetMute m control
