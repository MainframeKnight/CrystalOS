{-# LANGUAGE CApiFFI #-}
module CrystalOS.Filesystem where
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc (free)
foreign import capi "Basic.h file_exists" c_fileExists :: CString -> IO CInt
foreign import capi "Basic.h file_open" c_fileOpen :: CString -> CInt -> CInt -> CInt -> IO CInt
foreign import capi "Basic.h file_close" c_fileClose :: CInt -> IO CInt
foreign import capi "Basic.h file_write" c_fileWrite :: CInt -> CString -> IO CInt
foreign import capi "Basic.h file_readline" c_fileRead :: CInt -> IO CString
foreign import capi "Basic.h current_dir" c_currentDir :: IO CString
foreign import capi "Basic.h set_dir" c_setCurrentDir :: CString -> IO CInt
foreign import capi "Basic.h file_rename" c_rename :: CString -> CString -> IO CInt
foreign import capi "Basic.h create_dir" c_createDir :: CString -> IO CInt
foreign import capi "Basic.h delete_file" c_deleteFile :: CString -> IO CInt
foreign import capi "Basic.h delete_dir" c_deleteDir :: CString -> IO CInt
foreign import capi "Basic.h list_dir" c_listDir :: CString -> Ptr CInt -> IO (Ptr CString)
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

currentDir :: IO String
currentDir = do
    dir <- c_currentDir
    res <- peekCString dir
    free dir
    pure res

setCurrentDir :: String -> IO Bool
setCurrentDir dir = do
    temp_cstr <- newCString dir
    res <- c_setCurrentDir temp_cstr
    free temp_cstr
    pure (res == 0)

rename :: String -> String -> IO Bool
rename old new = do
    temp_cstr1 <- newCString old
    temp_cstr2 <- newCString new
    res <- c_rename temp_cstr1 temp_cstr2
    free temp_cstr1
    free temp_cstr2
    pure (res == 0)

createDir :: String -> IO Bool
createDir str = do
    temp_cstr <- newCString str
    res <- c_createDir temp_cstr
    free temp_cstr
    pure (res == 0)

deleteFile :: String -> IO Bool
deleteFile str = do
    temp_cstr <- newCString str
    res <- c_deleteFile temp_cstr
    free temp_cstr
    pure (res == 0)

data DirDeleteError = None | Error | HasFiles
deleteDir :: String -> IO DirDeleteError
deleteDir str = do
    temp_cstr <- newCString str
    res <- c_deleteDir temp_cstr
    free temp_cstr
    pure (case res of
        0 -> None
        1 -> HasFiles
        _ -> Error)

listDir :: String -> IO [String]
listDir str = do
    temp_cstr <- newCString str
    temp_int <- malloc :: IO (Ptr CInt)
    res <- c_listDir temp_cstr temp_int
    value <- fmap toInteger (peek temp_int)
    free temp_int
    res_list <- peekArray (fromInteger value) res
    free temp_cstr
    string_array <- mapM peekCString res_list
    mapM_ free res_list
    free res
    pure string_array