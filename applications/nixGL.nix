let settings = import ../settings.nix;
in pkgs: command:
if settings.nixOS then
  command
else
  let
    nixgl = (pkgs.callPackage "${
        builtins.fetchTarball {
          url = "https://github.com/guibou/nixGL/archive/main.tar.gz";
        }
      }/nixGL.nix" { });
  in "${nixgl.auto.nixGLDefault}/bin/nixGL ${command}"
