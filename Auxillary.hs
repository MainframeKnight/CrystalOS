{-# LANGUAGE CApiFFI #-}
module CrystalOS.Auxillary(
    getRandom32Int, getRandom64Int, command,
    getUNIXtime, getDateTime, sleep, getEnvironmentVar, getHomeDir
) where
import Foreign
import Foreign.C
foreign import capi "crystal_auxillary.h random_int" c_getRandomInt :: IO CInt
foreign import capi "crystal_auxillary.h random_long" c_getRandomLong :: IO CLong
foreign import capi "crystal_auxillary.h system" c_command :: CString -> IO CInt
foreign import capi "crystal_auxillary.h time" c_time :: Ptr CLong -> IO CLong
foreign import capi "crystal_auxillary.h get_datetime" c_getDateTime :: IO CString
foreign import capi "crystal_auxillary.h sleep" c_sleep :: CUInt -> IO CUInt
foreign import capi "crystal_auxillary.h getenv" c_getEnvVar :: CString -> IO CString

getRandom32Int :: IO Integer
getRandom32Int = fmap toInteger c_getRandomInt

getRandom64Int :: IO Integer
getRandom64Int = fmap toInteger c_getRandomLong

command :: String -> IO Bool
command cmd = do
    temp_cstr <- newCString cmd
    res <- c_command temp_cstr
    free temp_cstr
    pure (res == 0)

getUNIXtime :: IO Integer
getUNIXtime = fmap toInteger (c_time nullPtr)

getDateTime :: IO String
getDateTime = do
    res <- c_getDateTime
    str <- peekCString res
    free res
    pure str

sleep :: Integer -> IO ()
sleep n = do
    c_sleep (fromInteger n)
    pure ()

getEnvironmentVar :: String -> IO (Maybe String)
getEnvironmentVar var = do
    temp_cstr <- newCString var
    res <- c_getEnvVar temp_cstr
    free temp_cstr
    if res == nullPtr then pure Nothing else do
        e_var <- peekCString res
        pure (Just e_var)

getHomeDir :: IO (Maybe String)
getHomeDir = getEnvironmentVar "$HOME"