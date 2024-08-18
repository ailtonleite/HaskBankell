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
bindir     = "/home/lucas/HaskBankell/.stack-work/install/x86_64-linux/bc49cac8272e6412b33aff4365e46bbbdb2c7207c032b8f4dab377f99f95041f/9.6.6/bin"
libdir     = "/home/lucas/HaskBankell/.stack-work/install/x86_64-linux/bc49cac8272e6412b33aff4365e46bbbdb2c7207c032b8f4dab377f99f95041f/9.6.6/lib/x86_64-linux-ghc-9.6.6/projeto-hsl-0.1.0.0-7J8DKEoBfcY82R0B7hBOYp-my-tests"
dynlibdir  = "/home/lucas/HaskBankell/.stack-work/install/x86_64-linux/bc49cac8272e6412b33aff4365e46bbbdb2c7207c032b8f4dab377f99f95041f/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/lucas/HaskBankell/.stack-work/install/x86_64-linux/bc49cac8272e6412b33aff4365e46bbbdb2c7207c032b8f4dab377f99f95041f/9.6.6/share/x86_64-linux-ghc-9.6.6/projeto-hsl-0.1.0.0"
libexecdir = "/home/lucas/HaskBankell/.stack-work/install/x86_64-linux/bc49cac8272e6412b33aff4365e46bbbdb2c7207c032b8f4dab377f99f95041f/9.6.6/libexec/x86_64-linux-ghc-9.6.6/projeto-hsl-0.1.0.0"
sysconfdir = "/home/lucas/HaskBankell/.stack-work/install/x86_64-linux/bc49cac8272e6412b33aff4365e46bbbdb2c7207c032b8f4dab377f99f95041f/9.6.6/etc"

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
