# Fonts I like, in order of preference: MonoLisa, Cascadia Code, FiraCode, Dank Mono, JetBrains Mono
# Fonts to try: FantasqueSansMono, Inconsolata, Victor Mono
{ den, self, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      # Package my own customized version of Maple Mono with preferred features & stylistic sets,
      # to support editors like emacs that don't support toggling them.
      # To update the customizations, generate a new custom build here: https://font.subf.dev/en/playground/
      # then replace the ZIP.
      # baseline: ss03, ss10, ss11, calt
      packages.maple-mono-patched = pkgs.stdenvNoCC.mkDerivation {
        pname = "maple-mono-unhinted-patched";
        version = "1";
        src = ./MapleMono-NF-unhinted-patch.zip;
        nativeBuildInputs = [ pkgs.unzip ];
        dontUnpack = true;
        installPhase = ''
          mkdir -p $out/share/fonts/truetype
          unzip $src -d $out/share/fonts/truetype
        '';
      };

      packages.monolisa = pkgs.callPackage (builtins.fetchGit {
        url = "https://gitlab.com/Smona/monolisa.git";
        ref = "main";
        rev = "537b8613fc850cf0c90bb240c3412bb30f1e7b44";
      }) { };
    };

  den.aspects.fonts = {
    includes = [
      (den.batteries.unfree [
        "corefonts"
        "vista-fonts"
      ])
    ];
    homeManager = { pkgs, ... }: {
      home.packages =
        (with pkgs; [
          # required by Emacs for monospaced icons
          nerd-fonts.symbols-only
          noto-fonts # Current obsidian font
          noto-fonts-color-emoji # Required by Emacs
          source-serif # Required by Emacs
          source-sans
          rounded-mgenplus # Japanese font support
          # M$ fonts
          corefonts
          vista-fonts
        ])
        ++ (with self.packages.${pkgs.stdenv.hostPlatform.system}; [
          monolisa
          maple-mono-patched
        ]);
    };

    hmLinux = { ... }: {
      # Enable system to discover fonts installed by home-manager
      fonts.fontconfig.enable = true;
    };
  };
}
