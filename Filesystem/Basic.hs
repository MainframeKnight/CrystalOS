{-# LANGUAGE CApiFFI #-}
module CrystalOS.Filesystem.Basic where
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc (free)
foreign import capi "Basic.h file_exists" c_fileExists :: CString -> IO CInt
foreign import capi "Basic.h file_open" c_fileOpen :: CString -> CInt -> CInt -> CInt -> IO CInt
foreign import capi "Basic.h file_close" c_fileClose :: CInt -> IO CInt
foreign import capi "Basic.h file_write" c_fileWrite :: CInt -> CString -> IO CInt
foreign import capi "Basic.h file_readline" c_fileRead :: CInt -> IO CString
data File = File {
    path :: String,
    valid :: Bool,
    desc :: Integer
}
data AccessMode = Read | Write | ReadWrite | WriteAppend | ReadWriteAppend

fileExists :: String -> IO Bool
fileExists str = do
    temp_cstr <- newCString str;
    res <- fmap (==1) (c_fileExists temp_cstr);
    free temp_cstr;
    return res;

fileClose :: File -> IO Bool
fileClose f = if valid f then
        fmap (==0) (c_fileClose (fromIntegral (desc f)))
    else
        pure False

fileOpen :: String -> AccessMode -> IO File
fileOpen name ac = do
    temp_cstr <- newCString name
    fd <- fmap fromIntegral (case ac of
        Read -> c_fileOpen temp_cstr 1 0 1
        Write -> c_fileOpen temp_cstr 2 0 1
        ReadWrite -> c_fileOpen temp_cstr 3 0 1
        WriteAppend -> c_fileOpen temp_cstr 2 1 1
        ReadWriteAppend -> c_fileOpen temp_cstr 3 1 1)
    free temp_cstr
    pure File { path = name, valid = fd /= -1, desc = fd}

fileWrite :: File -> String -> IO Bool
fileWrite f str = do
    if valid f then do
        temp_cstr <- newCString str
        res <- c_fileWrite (fromIntegral (desc f)) temp_cstr
        free temp_cstr
        pure (res /= -1)
    else
        pure False

fileReadline :: File -> IO String
fileReadline f = do
    if valid f then do
        temp_cstr <- c_fileRead (fromIntegral (desc f))
        str <- peekCString temp_cstr
        free temp_cstr
        pure str
    else pure ""