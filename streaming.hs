import System.USB
import Control.Monad
import Data.Vector (toList, Vector)
import Control.Exception
import Text.PrettyPrint
import Control.Arrow hiding ((<+>))
import qualified Data.ByteString as BS
import Numeric
import Data.Char
import Data.List
import Text.Printf
import Data.Maybe
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad.Trans.State

isMidimanKeystation88es :: DeviceDesc -> Bool
isMidimanKeystation88es desc = True
{-isMidimanKeystation88es desc = deviceVendorId desc == midimanVID
                           && deviceProductId desc == keystation88esPID -}
    where
      midimanVID = 0x0763
      keystation88esPID = 0x0192

main = do
    ctx <- newCtx
    evalStateT (mainLoop ctx) Nothing

mainLoop:: Ctx -> ThreadHolder
mainLoop ctx = do
    lift $ threadDelay 200000 -- 200ms 
    connected <- lift $ isMidiConnected ctx
    prevConnected <- get
    case prevConnected of
        Nothing -> if not connected then return ()
                      else do devs <- lift $ fmap toList $ getDevices ctx
                              devDescs <- mapM (lift . getDeviceDesc) devs
                              let midi = findIndex isMidimanKeystation88es devDescs
                              case midi of Nothing -> return ()
                                           Just i -> do 
                                              newThread <- lift $ performConnection (devs!!i)
                                              put (Just newThread)
        Just t -> if connected then return ()
                      else do lift $ performDisconnection t
                              put Nothing
    mainLoop ctx
  where
    performConnection device = putStrLn "Connected" >> forkIO (deviceHandler device)
    performDisconnection thread = putStrLn "Disconnected" >> killThread thread
    
type ThreadHolder = StateT (Maybe ThreadId) IO ()

    
deviceHandler :: Device -> IO ()
deviceHandler device = catchUSBException go $ \e ->
       do putStr "Exception caught: " >> print e
    where go = do
            putStrLn "opening device..."
            handle <- openDevice device
            putStrLn "device opened"
            closeDevice handle
            putStrLn "device closed"
    
    
isMidiConnected :: Ctx -> IO Bool
isMidiConnected ctx = do
    devices <- toList <$> getDevices ctx
    descs <- mapM getDeviceDesc devices
    return $ length devices > 2
    --midis <- sequence $ zipWith getMidiConfig devices descs
    --return $ length (catMaybes midis) > 0

    
catchUSBException :: IO a -> (USBException -> IO a) -> IO a
catchUSBException = catch

getMidiConfig :: Device -> DeviceDesc -> IO (Maybe ConfigDesc)
getMidiConfig dev desc =
    do let numConfigs = deviceNumConfigs desc
       configs <- mapM (\c -> catchUSBException
                           (Just <$> getConfigDesc dev c)
                           (\e -> return Nothing)
                       )
                       [0..numConfigs-1]
       let realConfigs = catMaybes configs
       if length realConfigs == 0
        then return Nothing
        else let m = map (\a -> (a, getInterfaces a)) realConfigs
                 findC (cfg, ifaces) = find isMidi ifaces /= Nothing
             in return $ (fst <$> find findC m)
  where
    getInterfaces config =
        let interfaces = toList $ configInterfaces config
            alt'interfaces = map toList interfaces
        in join alt'interfaces
    isMidi interface = interfaceClass interface == 1
                        && interfaceSubClass interface == 3