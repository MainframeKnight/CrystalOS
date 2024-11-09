{-# LANGUAGE CApiFFI #-}
module CrystalOS.Thread where
import Foreign
import Foreign.C
foreign import capi "crystal_thread.h thread_create" c_createThread :: Ptr CULong -> FunPtr (Ptr () -> IO (Ptr ())) -> IO CInt
foreign import capi "crystal_thread.h thread_exit" c_exitThread :: IO ()
foreign import capi "crystal_thread.h thread_join" c_joinThread :: CULong -> IO CInt
foreign import capi "crystal_thread.h thread_detach" c_detachThread :: CULong -> IO CInt
foreign import capi "crystal_thread.h create_mutex" c_createMutex :: Ptr CInt -> IO (Ptr ())
foreign import capi "crystal_thread.h lock_mutex" c_lockMutex :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h trylock_mutex" c_trylockMutex :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h unlock_mutex" c_unlockMutex :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h destroy_mutex" c_destroyMutex :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h create_condvar" c_createCondvar :: Ptr CInt -> IO (Ptr ())
foreign import capi "crystal_thread.h destroy_condvar" c_destroyCondvar :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h signal_condvar" c_signalCondvar :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h broadcast_condvar" c_broadcastCondvar :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h wait_condvar" c_waitCondvar :: Ptr () -> Ptr () -> IO CInt
foreign import capi "crystal_thread.h timedwait_condvar" c_timedwaitCondvar :: Ptr () -> Ptr () -> CLong -> IO CInt
foreign import ccall "wrapper" mkFunc :: (Ptr () -> IO (Ptr ())) -> IO (FunPtr (Ptr () -> IO (Ptr ())))
type ThreadHandle = Integer
type Mutex = Ptr ()
type CondVar = Ptr ()

createThread :: IO () -> IO (Maybe ThreadHandle)
-- freeHaskellFunPtr or not? The System.Posix module doesn't.
createThread act = do
    hdl <- new 0 :: IO (Ptr CULong)
    let f_temp = nullFunPtr
    f_temp <- mkFunc (\p -> do act; pure nullPtr)
    ok <- c_createThread hdl f_temp
    if ok /= 0 then do
        free hdl
        pure Nothing 
    else do
        res <- peek hdl
        free hdl
        pure (Just (toInteger res))

exitThread :: IO ()
exitThread = c_exitThread

joinThread :: ThreadHandle -> IO Bool
joinThread hdl = do
    res <- c_joinThread (fromInteger hdl)
    pure (res == 0)

detachThread :: ThreadHandle -> IO Bool
detachThread hdl = do
    res <- c_detachThread (fromInteger hdl)
    pure (res == 0)

createMutex :: IO (Maybe Mutex)
createMutex = do
    temp_ptr <- new 0
    res <- c_createMutex temp_ptr
    ok <- peek temp_ptr
    free temp_ptr
    if ok == 0 then pure (Just res) else pure Nothing

lockMutex :: Mutex -> IO Bool
lockMutex mutex = do
    res <- c_lockMutex mutex
    pure (res == 0)

trylockMutex :: Mutex -> IO (Maybe Bool)
trylockMutex mutex = do
    res <- c_trylockMutex mutex
    case res of
        0 -> pure (Just True)
        1 -> pure (Just False)
        2 -> pure Nothing

unlockMutex :: Mutex -> IO Bool
unlockMutex mutex = do
    res <- c_unlockMutex mutex
    pure (res == 0)

destroyMutex :: Mutex -> IO Bool
destroyMutex mutex = do
    res <- c_destroyMutex mutex
    pure (res == 0)

createCondvar :: IO (Maybe CondVar)
createCondvar = do
    temp_ptr <- new 0
    res <- c_createCondvar temp_ptr
    ok <- peek temp_ptr
    free temp_ptr
    if ok == 0 then pure (Just res) else pure Nothing

signalCondvar :: CondVar -> IO Bool
signalCondvar cond = do
    res <- c_signalCondvar cond
    pure (res == 0)

broadcastCondvar :: CondVar -> IO Bool
broadcastCondvar cond = do
    res <- c_broadcastCondvar cond
    pure (res == 0)

waitCondvar :: CondVar -> Mutex -> IO Bool
waitCondvar condvar mtx = do
    res <- c_waitCondvar condvar mtx
    pure (res == 0)

timedWaitCondvar :: CondVar -> Mutex -> Integer -> IO Bool
timedWaitCondvar condvar mtx msec = do
    res <- c_timedwaitCondvar condvar mtx (fromInteger msec)
    pure (res == 0)

destroyCondvar :: CondVar -> IO Bool
destroyCondvar condvar = do
    res <- c_destroyCondvar condvar
    pure (res == 0)