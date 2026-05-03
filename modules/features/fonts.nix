# Fonts I like, in order of preference: MonoLisa, Cascadia Code, FiraCode, Dank Mono, JetBrains Mono
# Fonts to try: FantasqueSansMono, Inconsolata, Victor Mono
{ ... }:
{
  flake.homeModules.fonts =
    { pkgs, ... }:
    let
      monolisa = pkgs.callPackage (import ../../pkgs/monolisa.nix) { };
    in
    {
      home.packages = with pkgs; [
        # required by Emacs for monospaced icons
        nerd-fonts.symbols-only
        noto-fonts-color-emoji # Required by Emacs
        source-serif # Required by Emacs
        source-sans
        rounded-mgenplus # Japanese font support
        monolisa
        # M$ fonts
        corefonts
        vista-fonts
      ];
    };
}
