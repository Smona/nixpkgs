{ pkgs ? import <nixpkgs> { }, ... }:

let python = pkgs.python310;
in pkgs.stdenv.mkDerivation {
  pname = "tablet_mode_switch";
  version = "latest";

  nativeBuildInputs = with pkgs; [ ];
  buildInputs = [ python ];

  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    cp $src/tablet_mode_switch.py $out/bin/tablet_mode_switch
  '';
}
