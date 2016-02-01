module Paths_QualityMeasures (
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

bindir     = "/home/curt/Desktop/QualityMeasures/.stack-work/install/i386-linux/lts-5.1/7.10.3/bin"
libdir     = "/home/curt/Desktop/QualityMeasures/.stack-work/install/i386-linux/lts-5.1/7.10.3/lib/i386-linux-ghc-7.10.3/QualityMeasures-0.1.0.0-CHmY6cP4TPfDSmbbJXxsyB"
datadir    = "/home/curt/Desktop/QualityMeasures/.stack-work/install/i386-linux/lts-5.1/7.10.3/share/i386-linux-ghc-7.10.3/QualityMeasures-0.1.0.0"
libexecdir = "/home/curt/Desktop/QualityMeasures/.stack-work/install/i386-linux/lts-5.1/7.10.3/libexec"
sysconfdir = "/home/curt/Desktop/QualityMeasures/.stack-work/install/i386-linux/lts-5.1/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "QualityMeasures_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "QualityMeasures_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "QualityMeasures_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "QualityMeasures_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "QualityMeasures_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
