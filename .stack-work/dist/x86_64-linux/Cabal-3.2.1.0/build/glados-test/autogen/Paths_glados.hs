{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_glados (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/artorius/Delivery/Tek3/FUNC/B-FUN-500-PAR-5-2-glados-sacha.lliso/.stack-work/install/x86_64-linux/f93847ff253c437106e4969c3020abba0db66b35cd74cd8c8217b6e6c487a692/8.10.7/bin"
libdir     = "/home/artorius/Delivery/Tek3/FUNC/B-FUN-500-PAR-5-2-glados-sacha.lliso/.stack-work/install/x86_64-linux/f93847ff253c437106e4969c3020abba0db66b35cd74cd8c8217b6e6c487a692/8.10.7/lib/x86_64-linux-ghc-8.10.7/glados-0.1.0.0-Ck6oDEE5VkdEqK40a2FRNi-glados-test"
dynlibdir  = "/home/artorius/Delivery/Tek3/FUNC/B-FUN-500-PAR-5-2-glados-sacha.lliso/.stack-work/install/x86_64-linux/f93847ff253c437106e4969c3020abba0db66b35cd74cd8c8217b6e6c487a692/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/artorius/Delivery/Tek3/FUNC/B-FUN-500-PAR-5-2-glados-sacha.lliso/.stack-work/install/x86_64-linux/f93847ff253c437106e4969c3020abba0db66b35cd74cd8c8217b6e6c487a692/8.10.7/share/x86_64-linux-ghc-8.10.7/glados-0.1.0.0"
libexecdir = "/home/artorius/Delivery/Tek3/FUNC/B-FUN-500-PAR-5-2-glados-sacha.lliso/.stack-work/install/x86_64-linux/f93847ff253c437106e4969c3020abba0db66b35cd74cd8c8217b6e6c487a692/8.10.7/libexec/x86_64-linux-ghc-8.10.7/glados-0.1.0.0"
sysconfdir = "/home/artorius/Delivery/Tek3/FUNC/B-FUN-500-PAR-5-2-glados-sacha.lliso/.stack-work/install/x86_64-linux/f93847ff253c437106e4969c3020abba0db66b35cd74cd8c8217b6e6c487a692/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "glados_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "glados_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "glados_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "glados_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "glados_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "glados_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
