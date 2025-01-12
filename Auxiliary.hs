{-# LANGUAGE CApiFFI #-}
module CrystalOS.Auxiliary(
    getRandom32UInt, getRandom64UInt, getRandomInRange, command,
    getUNIXtime, getDateTime, sleep, getEnvironmentVar, getHomeDir
) where
import Foreign
import Foreign.C
foreign import capi "crystal_auxiliary.h random_uint" c_getRandomUInt :: IO CUInt
foreign import capi "crystal_auxiliary.h random_ulong" c_getRandomULong :: IO CULong
foreign import capi "crystal_auxiliary.h random_unbiased_uint" c_getRandomUnbiasedUInt :: CUInt -> CUInt -> IO CUInt
foreign import capi "crystal_auxiliary.h system" c_command :: CString -> IO CInt
foreign import capi "crystal_auxiliary.h time" c_time :: Ptr CLong -> IO CLong
foreign import capi "crystal_auxiliary.h get_datetime" c_getDateTime :: IO CString
foreign import capi "crystal_auxiliary.h sleep" c_sleep :: CUInt -> IO CUInt
foreign import capi "crystal_auxiliary.h getenv" c_getEnvVar :: CString -> IO CString

getRandom32UInt :: IO Integer
getRandom32UInt = fmap toInteger c_getRandomUInt

getRandom64UInt :: IO Integer
getRandom64UInt = fmap toInteger c_getRandomULong

-- Overflow if args are greater than C unsigned int maximal value.
getRandomInRange :: Integer -> Integer -> IO Integer
getRandomInRange a b = fmap toInteger (c_getRandomUnbiasedUInt (fromInteger a) (fromInteger b))

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