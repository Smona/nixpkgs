{ pkgs ? import <nixpkgs> { }, ... }:

let python = pkgs.python310.withPackages (p: with p; [ pygobject3 ]);
in pkgs.stdenv.mkDerivation {
  pname = "hypr_info";
  version = "1.0";

  nativeBuildInputs = with pkgs; [ gobject-introspection wrapGAppsHook ];
  buildInputs = [ python pkgs.gtk3 ];

  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    cp $src/hypr_info.py $out/bin/hypr_info
  '';
}
