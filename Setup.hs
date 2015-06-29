import Distribution.PackageDescription (HookedBuildInfo)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{preBuild = hook}

hook :: Args -> BuildFlags -> IO HookedBuildInfo
hook args flags = do
    system "make sq"
    return (Nothing, [])
