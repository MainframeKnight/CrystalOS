# CrystalOS
**CrystalOS** is a library for interacting with Linux operating system for
Haskell programming language. The functions in CrystalOS provide abstractions over
their C-equivalents (C is used in Linux system programming) so that they can be
used in a usual way in a functional programming environment.

The library is split in 4 logical parts corresponding to different functions
of OS usually needed by user-space programs. These modules are:
- *CrystalOS.Filesystem*: provides interface for working with files and directories.
- *CrystalOS.Thread*: provides interface for multithreading 
and thread synchronization (the supported synchronization primitives are mutexes, semaphores, condition variables).
*When compiling programs using this module with GHC it is necessary to add the flag '-threaded' for multithreading support.*
- *CrystalOS.Process*: provides interface for working with processes + the *fork()* function for creating processes.
- *CrystalOS.Auxilliary*: provides miscellaneous functions of OS environment like cryptographically-secure
 random number generation, time functions and a function for executing commands in the OS shell.
