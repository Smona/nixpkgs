{
  perSystem =
    {
      pkgs ? import <nixpkgs> { },
      ...
    }:

    let
      python = pkgs.python3;
    in
    {
      packages.tablet_mode_switch = pkgs.stdenv.mkDerivation {
        pname = "tablet_mode_switch";
        version = "latest";

        nativeBuildInputs = [ ];
        buildInputs = [ python ];

        src = ./.;

        installPhase = ''
          mkdir -p $out/bin
          cp $src/tablet_mode_switch.py $out/bin/tablet_mode_switch
        '';
      };
    };
}
