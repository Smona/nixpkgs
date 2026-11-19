{
  perSystem =
    { pkgs, ... }:
    {
      packages.pi-acp = pkgs.buildNpmPackage rec {
        pname = "pi-acp";
        version = "0.0.34";

        src = pkgs.fetchFromGitHub {
          owner = "svkozak";
          repo = "pi-acp";
          tag = "v${version}";
          hash = "sha256-QRwxOtTZOY+Np3PkAoy2o2PrUzEqjItM/372sCPlSMo=";
        };
        npmDepsHash = "sha256-BvLNtFfp1cMVjzWcMRSdhTqiJrTfbFoUbWkkPW9200o=";
      };
    };
}
