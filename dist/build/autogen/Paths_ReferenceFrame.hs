module Paths_ReferenceFrame (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/juergen/workspace/ReferenceFrame/.cabal-sandbox/bin"
libdir     = "/home/juergen/workspace/ReferenceFrame/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.3/ReferenceFrame-0.1.0.0"
datadir    = "/home/juergen/workspace/ReferenceFrame/.cabal-sandbox/share/x86_64-linux-ghc-7.8.3/ReferenceFrame-0.1.0.0"
libexecdir = "/home/juergen/workspace/ReferenceFrame/.cabal-sandbox/libexec"
sysconfdir = "/home/juergen/workspace/ReferenceFrame/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ReferenceFrame_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ReferenceFrame_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ReferenceFrame_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ReferenceFrame_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ReferenceFrame_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
