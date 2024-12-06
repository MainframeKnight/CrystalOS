{-# LANGUAGE CApiFFI #-}
module CrystalOS.Thread (
    ThreadHandle, Mutex, CondVar, Semaphore,
    createThread, exitThread, joinThread, detachThread, getThreadID,
    createMutex, lockMutex, unlockMutex, trylockMutex, destroyMutex,
    createCondvar, signalCondvar, broadcastCondvar, waitCondvar, waitForCond, 
    timedWaitCondvar, destroyCondvar, createSemaphore, incrementSemaphore,
    getValSemaphore, waitSemaphore, trywaitSemaphore, destroySemaphore
) where
import Foreign
import Foreign.C
foreign import capi "crystal_thread.h thread_create" c_createThread :: Ptr CULong -> FunPtr (Ptr () -> IO (Ptr ())) -> IO CInt
foreign import capi "crystal_thread.h thread_exit" c_exitThread :: IO ()
foreign import capi "crystal_thread.h thread_join" c_joinThread :: CULong -> IO CInt
foreign import capi "crystal_thread.h thread_detach" c_detachThread :: CULong -> IO CInt
foreign import capi "crystal_thread.h gettid" c_getThreadID :: IO CInt
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
foreign import capi "crystal_thread.h create_semaphore" c_createSemaphore :: CInt -> Ptr CInt -> IO (Ptr ())
foreign import capi "crystal_thread.h increment_semaphore" c_incrSemaphore :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h get_semaphore" c_getSemaphore :: Ptr () -> Ptr CInt -> IO CInt
foreign import capi "crystal_thread.h wait_dec_semaphore" c_waitSemaphore :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h try_wait_dec_semaphore" c_trywaitSemaphore :: Ptr () -> IO CInt
foreign import capi "crystal_thread.h destroy_semaphore" c_destroySemaphore :: Ptr () -> IO CInt
foreign import ccall "wrapper" mkFunc :: (Ptr () -> IO (Ptr ())) -> IO (FunPtr (Ptr () -> IO (Ptr ())))
type ThreadHandle = Integer
newtype Mutex = Mutex (Ptr ())
newtype CondVar = CondVar (Ptr ())
newtype Semaphore = Semaphore (Ptr ())

createThread :: IO () -> IO (ThreadHandle, Bool)
-- freeHaskellFunPtr or not? The System.Posix module doesn't.
createThread act = do
    hdl <- new 0 :: IO (Ptr CULong)
    let f_temp = nullFunPtr
    f_temp <- mkFunc (\p -> do act; pure nullPtr)
    ok <- c_createThread hdl f_temp
    if ok /= 0 then do
        free hdl
        pure (0, False)
    else do
        res <- peek hdl
        free hdl
        pure (toInteger res, True)

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

getThreadID :: IO Integer
getThreadID = do toInteger <$> c_getThreadID

createMutex :: IO (Mutex, Bool)
createMutex = do
    temp_ptr <- new 0
    res <- c_createMutex temp_ptr
    ok <- peek temp_ptr
    free temp_ptr
    if ok == 0 then pure (Mutex res, True) else pure (Mutex nullPtr, False)

lockMutex :: Mutex -> IO Bool
lockMutex mutex = do
    case mutex of
        Mutex ptr -> do
            res <- c_lockMutex ptr
            pure (res == 0)

trylockMutex :: Mutex -> IO (Maybe Bool)
trylockMutex mutex = do
    case mutex of
        Mutex ptr -> do
            res <- c_trylockMutex ptr
            case res of
                0 -> pure (Just True)
                1 -> pure (Just False)
                2 -> pure Nothing

unlockMutex :: Mutex -> IO Bool
unlockMutex mutex = do
    case mutex of
        Mutex ptr -> do
            res <- c_unlockMutex ptr
            pure (res == 0)

destroyMutex :: Mutex -> IO Bool
destroyMutex mutex = do
    case mutex of
        Mutex ptr -> do
            res <- c_destroyMutex ptr
            pure (res == 0)

createCondvar :: IO (CondVar, Bool)
createCondvar = do
    temp_ptr <- new 0
    res <- c_createCondvar temp_ptr
    ok <- peek temp_ptr
    free temp_ptr
    if ok == 0 then pure (CondVar res, True) else pure (CondVar nullPtr, False)

signalCondvar :: CondVar -> IO Bool
signalCondvar cond = do
    case cond of
        CondVar cv -> do
            res <- c_signalCondvar cv
            pure (res == 0)

broadcastCondvar :: CondVar -> IO Bool
broadcastCondvar cond = do
    case cond of
        CondVar cv -> do
            res <- c_broadcastCondvar cv
            pure (res == 0)

waitCondvar :: CondVar -> Mutex -> IO Bool
waitCondvar condvar mtx = do
    case condvar of
        CondVar cv -> do
            case mtx of
                Mutex mt -> do
                    res <- c_waitCondvar cv mt
                    pure (res == 0)

timedWaitCondvar :: CondVar -> Mutex -> Integer -> IO Bool
timedWaitCondvar condvar mtx msec = do
    case condvar of
        CondVar cv -> do
            case mtx of
                Mutex mt -> do
                    res <- c_timedwaitCondvar cv mt (fromInteger msec)
                    pure (res == 0)

destroyCondvar :: CondVar -> IO Bool
destroyCondvar condvar = do
    case condvar of
        CondVar cv -> do
            res <- c_destroyCondvar cv
            pure (res == 0)

waitForCond :: CondVar -> Mutex -> IO Bool -> IO ()
waitForCond condvar mtx pred = do
    lockMutex mtx
    waitWhileNotLoop condvar mtx pred
    unlockMutex mtx
    pure ()

waitWhileNotLoop :: CondVar -> Mutex -> IO Bool -> IO ()
waitWhileNotLoop condvar mtx pred = do
    val <- pred
    if val then do
        pure ()
    else do
        waitCondvar condvar mtx
        waitWhileNotLoop condvar mtx pred

createSemaphore :: Integer -> IO (Semaphore, Bool)
createSemaphore val = do
    temp_ptr <- new 0
    res <- c_createSemaphore (fromInteger val) temp_ptr
    ok <- peek temp_ptr
    free temp_ptr
    pure (Semaphore res, ok == 0)

incrementSemaphore :: Semaphore -> IO Bool
incrementSemaphore sema = do
    case sema of
        Semaphore sem_ptr -> do
            ok <- c_incrSemaphore sem_ptr
            pure (ok == 0)

getValSemaphore :: Semaphore -> IO (Integer, Bool)
getValSemaphore sema = do
    case sema of
        Semaphore sem_ptr -> do
            temp_ptr <- new 0
            ok <- c_getSemaphore sem_ptr temp_ptr
            res <- peek temp_ptr
            free temp_ptr
            pure (fromIntegral res, ok == 0)

waitSemaphore :: Semaphore -> IO Bool
waitSemaphore sema = do
    case sema of
        Semaphore sem_ptr -> do
            ok <- c_waitSemaphore sem_ptr
            pure (ok == 0)

trywaitSemaphore :: Semaphore -> IO (Maybe Bool)
trywaitSemaphore sema = do
    case sema of
        Semaphore sem_ptr -> do
            res <- c_trywaitSemaphore sem_ptr
            case res of
                0 -> pure (Just True)
                1 -> pure (Just False)
                2 -> pure Nothing

destroySemaphore :: Semaphore -> IO Bool
destroySemaphore sema = do
    case sema of
        Semaphore sem_ptr -> do
            ok <- c_destroySemaphore sem_ptr
            pure (ok == 0)