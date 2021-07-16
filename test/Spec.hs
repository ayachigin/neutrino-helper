
import Lib
import System.Directory ( setCurrentDirectory )
import System.IO

main :: IO ()
main = do
    setCurrentDirectory "C:/Users/tetsuroh/Freeware/NEUTRINO/NEUTRINO"
    hPutStrLn stdout "1"
    m <- modelYouUse
    print m