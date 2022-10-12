# TODO: install as a flake
{ pkgs, isNixOS }:
command:
if isNixOS then
  command
else
  let
    nixgl = (pkgs.callPackage "${
        builtins.fetchTarball {
          url = "https://github.com/guibou/nixGL/archive/main.tar.gz";
        }
      }/nixGL.nix" { });
  in "${nixgl.auto.nixGLDefault}/bin/nixGL ${command}"
