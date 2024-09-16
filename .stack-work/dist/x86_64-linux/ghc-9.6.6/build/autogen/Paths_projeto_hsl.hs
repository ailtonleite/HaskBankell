{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_projeto_hsl (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ailton/ufabc/projeto-hsl/.stack-work/install/x86_64-linux/cce98880a57214800d17e0ee4df832f570cc6b09fa46ac01b3f9c230bbed2e98/9.6.6/bin"
libdir     = "/home/ailton/ufabc/projeto-hsl/.stack-work/install/x86_64-linux/cce98880a57214800d17e0ee4df832f570cc6b09fa46ac01b3f9c230bbed2e98/9.6.6/lib/x86_64-linux-ghc-9.6.6/projeto-hsl-0.1.0.0-1HTCbgkPako7PcFoSBWIR6"
dynlibdir  = "/home/ailton/ufabc/projeto-hsl/.stack-work/install/x86_64-linux/cce98880a57214800d17e0ee4df832f570cc6b09fa46ac01b3f9c230bbed2e98/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/ailton/ufabc/projeto-hsl/.stack-work/install/x86_64-linux/cce98880a57214800d17e0ee4df832f570cc6b09fa46ac01b3f9c230bbed2e98/9.6.6/share/x86_64-linux-ghc-9.6.6/projeto-hsl-0.1.0.0"
libexecdir = "/home/ailton/ufabc/projeto-hsl/.stack-work/install/x86_64-linux/cce98880a57214800d17e0ee4df832f570cc6b09fa46ac01b3f9c230bbed2e98/9.6.6/libexec/x86_64-linux-ghc-9.6.6/projeto-hsl-0.1.0.0"
sysconfdir = "/home/ailton/ufabc/projeto-hsl/.stack-work/install/x86_64-linux/cce98880a57214800d17e0ee4df832f570cc6b09fa46ac01b3f9c230bbed2e98/9.6.6/etc"

getBinDir     = catchIO (getEnv "projeto_hsl_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "projeto_hsl_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "projeto_hsl_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "projeto_hsl_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projeto_hsl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projeto_hsl_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'