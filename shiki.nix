{ mkDerivation, aeson, async, base, bytestring, containers
, directory, filepath, http-client, http-client-tls, http-types
, lib, optparse-applicative, process, stm, tasty, tasty-hunit, text
, time, unix, vector, wai, warp
}:
mkDerivation {
  pname = "shiki";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers directory filepath
    http-client http-client-tls http-types process stm text time unix
    vector wai warp
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [
    aeson base containers directory filepath http-client tasty
    tasty-hunit text time
  ];
  description = "Small probes that report back, as JSON for a dashboard";
  license = lib.meta.getLicenseFromSpdxId "MIT";
  mainProgram = "shiki";
}
