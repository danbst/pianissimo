module 
    UsbPretty 
    {-(
    )-}
    where

import System.USB
import Control.Monad (forM_, when)
import Data.Vector (toList, Vector)
import Control.Exception
import Text.PrettyPrint
import Control.Arrow hiding ((<+>))
import qualified Data.ByteString as BS
import Numeric
import Data.Char
import Data.List
import Text.Printf
import Data.Maybe (maybe)
import System.USB.IDDB

catchUSBException :: IO a -> (USBException -> IO a) -> IO a
catchUSBException = catch

textS :: (Show a) => a -> Doc
textS = text . show

columns :: Int -> [[Doc]] -> Doc
columns s rows = vcat . map (vcat . zipWith nest indents) $ rows
    where indents = init . scanl (\a b -> a + b + s) 0 $ columnSizes rows

columnSizes :: [[Doc]] -> [Int]
columnSizes = map (maximum . map docLen) . transpose

docLen :: Doc -> Int
docLen = length . render

byteToHex :: Word8 -> String
byteToHex x = printf "0x%02x" x

wordToHex :: Word16 -> String
wordToHex x = printf "0x%04x" x

prettyPrintByteString :: BS.ByteString -> Doc
prettyPrintByteString s =
    let stringRepr = show s
        hexRepr = intercalate " " $ map byteToHex $ BS.unpack s
    in if BS.null s
        then empty 
        else vcat [text stringRepr, text hexRepr]

{-
layout2columns :: [(String, Doc)] -> Doc
layout2columns lst = vcat . map docF $ lst
    where
        firstColumnLength = maximum (map (length . fst) lst)
        docF (label, value) = 
            if isEmpty value
                then empty
                else text label $$ nest (firstColumnLength + 1) value
-}
                
pretty'EndpointAddress :: EndpointAddress -> Doc
pretty'EndpointAddress address =
    let endpoint = textS (endpointNumber address)
        direction = pretty'TD (transferDirection address)
    in endpoint <> " - " <> direction
  where
    pretty'TD Out = text "Out (host -> device)"
    pretty'TD In  = text "In (device -> host)"

pretty'MaxPacketSize :: MaxPacketSize -> Doc
pretty'MaxPacketSize packetSize =
    let maxSize = textS (maxPacketSize packetSize)
        transactions = pretty'T (transactionOpportunities packetSize)
    in maxSize <> " - " <> transactions
  where   
    pretty'T Zero = text "Zero additional transactions"
    pretty'T One  = text "One additional transaction"
    pretty'T Two  = text "Two additional transactions"

pretty'EndpointDesc :: Maybe (ByteString -> Doc) -> EndpointDesc -> Doc
pretty'EndpointDesc extraPrettifier endpoint =
    let direction = transferDirection . endpointAddress $ endpoint
        header | direction == In = text "IN Endpoint"
        header | direction == Out = text "OUT Endpoint"
        address = pretty'EndpointAddress (endpointAddress endpoint)
        attrs = textS (endpointAttribs endpoint)
        packet = pretty'MaxPacketSize (endpointMaxPacketSize endpoint)
        interval = showInterval . fromIntegral . endpointInterval $ endpoint
        refresh = textS (endpointRefresh endpoint)
        synch = textS (endpointSynchAddress endpoint)
        extra = prettyExtra (endpointExtra endpoint)
    in header $+$ nest 2 
         (columns 1 [
             [text "Address:", address]
           , [text "Attributes:", attrs]
           , [text "Max packet size:", packet]
           , [text "Interval:", interval]
           , [text "Refresh rate:", refresh]
           , [text "Synch address:", synch]
           ]
           $+$ text "extra:" <+> extra
         )
  where
    showInterval i = hcat [int i, text "ms or ", int (i * 125), text "us"]
    prettyExtra = maybe pretty'ByteString id extraPrettifier

pretty'InterfaceDesc :: Maybe (ByteString -> Doc) -> InterfaceDesc -> Doc
pretty'InterfaceDesc extraPrettifier interface =
    let class' = textS (interfaceClass interface)
        subClass = textS (interfaceSubClass interface)
        protocol = textS (interfaceProtocol interface)
        extra = prettyExtra (interfaceExtra interface)
    in columns 1 [
             [text "Class:", class']
           , [text "Sub class:", subClass]
           , [text "Protocol:", protocol]
           ]
           $+$ text "extra:" <+> extra
  where
    showEndpoints = prettyShowList "Endpoint" prettyShowEndpointDesc
    prettyExtra = maybe pretty'ByteString id extraPrettifier

prettyShowConfigDesc :: ConfigDesc -> Doc
prettyShowConfigDesc config =
    layout2columns (map (second ($ config)) [
         ("value:", text . show . configValue)
       , ("StrIx:", maybe empty (text . show) . configStrIx)
       , ("attributes:", text . show . configAttribs)
       , ("max power:", (<> text "mA") . text . show . (*2) . configMaxPower)
       , ("extra:", prettyPrintByteString . configExtra)
      ])
    $+$ (showInterfaces . toList . configInterfaces $ config)
  where showInterfaces = prettyShowList "Interface" showInterfaceDesc
        showInterfaceDesc = prettyShowList "Alt interface" pretty'InterfaceDesc Nothing . toList

prettyShowList :: String -> (a -> Doc) -> [a] -> Doc
prettyShowList label f =
    let docF (i, value) = firstPart $+$ secondPart
          where firstPart = hcat [text label, text " #", int i]
                secondPart = nest 4 (f value)
    in vcat . map docF . zip [0..]
    