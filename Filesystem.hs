{-# LANGUAGE CApiFFI #-}
module CrystalOS.Filesystem (
    File, FilePermission(..), AccessMode(..), FileType(..),
    fileExists, fileClose, fileOpen, fileWrite, fileReadline,
    currentDir, setCurrentDir, rename, createDir, deleteFile, deleteDir, listDir,
    executeInDir, hardLink, symLink, readSymlink, getFilePermissions, getFileType
) where
import Foreign
import Foreign.C
import Foreign.Marshal.Alloc (free)
foreign import capi "crystal_filesystem.h file_exists" c_fileExists :: CString -> IO CInt
foreign import capi "crystal_filesystem.h file_open" c_fileOpen :: CString -> CInt -> CInt -> CInt -> IO CInt
foreign import capi "crystal_filesystem.h file_close" c_fileClose :: CInt -> IO CInt
foreign import capi "crystal_filesystem.h file_read" c_fileRead :: CInt -> Ptr CInt -> CULong -> IO CString
foreign import capi "crystal_filesystem.h file_write" c_fileWrite :: CInt -> CString -> IO CInt
foreign import capi "crystal_filesystem.h file_readline" c_fileReadline :: CInt -> IO CString
foreign import capi "crystal_filesystem.h current_dir" c_currentDir :: IO CString
foreign import capi "crystal_filesystem.h set_dir" c_setCurrentDir :: CString -> IO CInt
foreign import capi "crystal_filesystem.h file_rename" c_rename :: CString -> CString -> IO CInt
foreign import capi "crystal_filesystem.h create_dir" c_createDir :: CString -> IO CInt
foreign import capi "crystal_filesystem.h delete_file" c_deleteFile :: CString -> IO CInt
foreign import capi "crystal_filesystem.h delete_dir" c_deleteDir :: CString -> IO CInt
foreign import capi "crystal_filesystem.h list_dir" c_listDir :: CString -> Ptr CInt -> IO (Ptr CString)
foreign import capi "crystal_filesystem.h hard_link" c_hardLink :: CString -> CString -> IO CInt
foreign import capi "crystal_filesystem.h symbolic_link" c_symLink :: CString -> CString -> IO CInt
foreign import capi "crystal_filesystem.h read_symlink" c_readSymlink :: CString -> IO CString
foreign import capi "crystal_filesystem.h test_perm" c_testPerm :: CString -> CInt -> IO CInt
foreign import capi "crystal_filesystem.h file_type" c_fileType :: CString -> IO CInt
data File = File {
    path :: String,
    valid :: Bool,
    desc :: Integer
}
data FilePermission = ReadPerm | WritePerm | ExecutePerm deriving (Eq, Show)
data AccessMode = Read | Write | ReadWrite | WriteAppend | ReadWriteAppend deriving (Eq, Show)
data FileType = Directory | CharSpecial | BlockSpecial | Regular | FIFO | Link | Socket | Other deriving (Eq, Show)

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

fileRead :: File -> Integer -> IO (Bool, String)
fileRead f n_bytes = do
    if valid f then do
        temp_int <- new 0 :: IO (Ptr CInt)
        temp_cstr <- c_fileRead (fromInteger (desc f)) temp_int (fromInteger n_bytes)
        str <- peekCString temp_cstr
        ok <- peek temp_int
        free temp_cstr
        free temp_int
        pure (ok > 0, str)
    else pure (False, "")

fileReadline :: File -> IO String
fileReadline f = do
    if valid f then do
        temp_cstr <- c_fileReadline (fromIntegral (desc f))
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

executeInDir :: String -> IO b -> IO (Maybe b)
executeInDir dirpath f = do
    curPath <- currentDir
    ok <- setCurrentDir dirpath
    if not ok then
        pure Nothing
    else do
        res <- f
        setCurrentDir curPath
        pure (Just res)

hardLink :: String -> String -> IO Bool
hardLink old new = do
    temp_cstr1 <- newCString old
    temp_cstr2 <- newCString new
    res <- c_hardLink temp_cstr1 temp_cstr2
    free temp_cstr1
    free temp_cstr2
    pure (res == 0)

symLink :: String -> String -> IO Bool
symLink old new = do
    temp_cstr1 <- newCString old
    temp_cstr2 <- newCString new
    res <- c_symLink temp_cstr1 temp_cstr2
    free temp_cstr1
    free temp_cstr2
    pure (res == 0)

readSymlink :: String -> IO (Maybe String)
readSymlink link = do
    temp_cstr1 <- newCString link
    res <- c_readSymlink temp_cstr1
    free temp_cstr1
    if res == nullPtr then pure Nothing
    else do
        str <- peekCString res
        free res
        pure (Just str)

getFilePermissions :: String -> IO [FilePermission]
getFilePermissions name = do
    temp_cstr1 <- newCString name
    res_r <- c_testPerm temp_cstr1 4
    res_w <- c_testPerm temp_cstr1 2
    res_x <- c_testPerm temp_cstr1 1
    free temp_cstr1
    let tmp_array = [(ReadPerm, res_r), (WritePerm, res_w), (ExecutePerm, res_x)]
    pure [fst x | x <- tmp_array, snd x == 0]

getFileType :: String -> IO (Maybe FileType)
getFileType name = do
    temp_cstr1 <- newCString name
    res <- c_fileType temp_cstr1
    if res == -1 then pure Nothing else do
        free temp_cstr1
        pure (Just (case res of
            1 -> Directory
            2 -> CharSpecial
            3 -> BlockSpecial
            4 -> Regular
            5 -> FIFO
            6 -> Link
            7 -> Socket
            _ -> Other))