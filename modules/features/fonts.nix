# Fonts I like, in order of preference: MonoLisa, Cascadia Code, FiraCode, Dank Mono, JetBrains Mono
# Fonts to try: FantasqueSansMono, Inconsolata, Victor Mono
{ self, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.monolisa = pkgs.callPackage (builtins.fetchGit {
        url = "https://gitlab.com/Smona/monolisa.git";
        ref = "main";
        rev = "537b8613fc850cf0c90bb240c3412bb30f1e7b44";
      }) { };
    };

  flake.homeModules.fonts =
    { pkgs, ... }:
    {
      home.packages =
        (with pkgs; [
          # required by Emacs for monospaced icons
          nerd-fonts.symbols-only
          noto-fonts-color-emoji # Required by Emacs
          source-serif # Required by Emacs
          source-sans
          rounded-mgenplus # Japanese font support
          # M$ fonts
          corefonts
          vista-fonts
        ])
        ++ [ self.packages.${pkgs.stdenv.hostPlatform.system}.monolisa ];
    };
}
