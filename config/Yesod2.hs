{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withYesod2)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withYesod2 $ run 3000
#else
import Controller (withYesod2)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withYesod2 $ run port . debug
#endif
