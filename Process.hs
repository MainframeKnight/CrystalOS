{-# LANGUAGE CApiFFI #-}
module CrystalOS.Process (
    Status(..), getProcessID, getGeneratorID, fork, vFork, exitProcess,
    execute, executeArgs, executePATH, executeArgsPATH,
    waitGenerated, waitGeneratedPID, generateProcess, listProcessesPID
) where
import Foreign
import Foreign.C
import CrystalOS.Filesystem(listDir)
import Text.Read (readMaybe)
foreign import capi "crystal_process.h getpid" c_getProcessID :: IO CInt
foreign import capi "crystal_process.h getppid" c_getGeneratorID :: IO CInt
foreign import capi "crystal_process.h fork" c_Fork :: IO CInt
foreign import capi "crystal_process.h vfork" c_vFork :: IO CInt
foreign import capi "crystal_process.h exit" c_Exit :: CInt -> IO ()
foreign import capi "crystal_process.h execute_file_args" c_executeArgs :: CString -> Ptr CString -> IO CInt
foreign import capi "crystal_process.h execute_file_args_PATH" c_executeArgsPATH :: CString -> Ptr CString -> IO CInt
foreign import capi "crystal_process.h execute_file" c_execute :: CString -> IO CInt
foreign import capi "crystal_process.h execute_file_PATH" c_executePATH :: CString -> IO CInt
foreign import capi "crystal_process.h wait_generated" c_waitGenerated :: IO CInt
foreign import capi "crystal_process.h wait_generated_pid" c_waitGeneratedPID :: CInt -> IO CInt
data Status = Exited | Stopped | Signaled

getProcessID :: IO Integer
getProcessID = fmap toInteger c_getProcessID

getGeneratorID :: IO Integer
getGeneratorID = fmap toInteger c_getGeneratorID

fork :: IO Integer
fork = fmap toInteger c_Fork

vFork :: IO Integer
vFork = fmap toInteger c_vFork

exitProcess :: Integer -> IO ()
exitProcess n = c_Exit (fromInteger n)

execute :: String -> IO Integer
execute filename = do
    temp_str <- newCString filename
    res <- c_execute temp_str
    free temp_str
    pure (toInteger res)

executePATH :: String -> IO Integer
executePATH filename = do
    temp_str <- newCString filename
    res <- c_executePATH temp_str
    free temp_str
    pure (toInteger res)

executeArgs :: String -> [String] -> IO Integer
executeArgs filename args = do
    temp_str <- newCString filename
    c_list <- mapM newCString args
    temp_args <- newArray c_list
    res <- c_executeArgs temp_str temp_args
    free temp_str
    mapM_ free c_list
    free temp_args
    pure (toInteger res)

executeArgsPATH :: String -> [String] -> IO Integer
executeArgsPATH filename args = do
    temp_str <- newCString filename
    c_list <- mapM newCString args
    temp_args <- newArray c_list
    res <- c_executeArgsPATH temp_str temp_args
    free temp_str
    mapM_ free c_list
    free temp_args
    pure (toInteger res)

waitGenerated :: IO Status
waitGenerated = do
    res <- fmap toInteger c_waitGenerated
    case res of
        1 -> pure Exited
        2 -> pure Stopped
        3 -> pure Signaled

waitGeneratedPID :: Integer -> IO Status
waitGeneratedPID pid = do
    res <- fmap toInteger (c_waitGeneratedPID (fromInteger pid))
    case res of
        1 -> pure Exited
        2 -> pure Stopped
        3 -> pure Signaled

generateProcess :: IO () -> IO ()
generateProcess act = do
    res <- fork
    if res == 0 then 
        do act; exitProcess 0
    else pure ()

listProcessesPID :: IO [Integer]
listProcessesPID = do
    proc_fold <- listDir "/proc/"
    let pid_list = filter (\ent -> (readMaybe ent :: Maybe Integer) /= Nothing) proc_fold
    pure (fmap (\x -> read x :: Integer) pid_list)