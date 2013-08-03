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

main = do
    putStrLn "Scanning..."
    ctx <- newCtx
    mainLoop ctx False
    putStrLn "Finished!"

mainLoop:: Ctx -> Bool -> IO ()
mainLoop ctx prevConn = do
    threadDelay 2000000 -- 200ms 
    connected <- isMidiConnected ctx
    if (prevConn && not connected)
        then putStrLn "Disconnected"
        else if (not prevConn && connected)
                then putStrLn "Connected"
                else return ()
    mainLoop ctx connected

isMidiConnected :: Ctx -> IO Bool
isMidiConnected ctx = do
    devices <- toList <$> getDevices ctx
    descs <- mapM getDeviceDesc devices
    midis <- sequence $ zipWith getMidiConfig devices descs
    return $ length (catMaybes midis) > 0

    
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