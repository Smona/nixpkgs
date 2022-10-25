{ config, lib, pkgs, ... }:

let emacs = pkgs.emacsPgtkNativeComp;
in {
  programs.emacs = {
    enable = true;
    package = emacs;
    extraPackages = epkgs: [ epkgs.vterm epkgs.pdf-tools ];
  };

  services.emacs = {
    enable = true;
    package = emacs;
  };

  home.packages = with pkgs; [
    # Python
    python310Packages.debugpy # Required by dap-mode
    nodePackages.pyright

    # Nix
    rnix-lsp # Required by (nix +lsp)
    nixfmt # For emacs code formatting

    # Code Formatters
    nodePackages.prettier
    shfmt

    # Language servers
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.dockerfile-language-server-nodejs
    python310Packages.grip # Required by grip-mode (markdown +grip)

    # Assorted Emacs dependencies
    wakatime
  ];
}
