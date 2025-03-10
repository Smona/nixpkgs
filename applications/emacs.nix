{
  config,
  pkgs,
  inputs,
  ...
}:

let
  emacs = pkgs.emacs-pgtk;
  my-emacs = (
    config.lib.nixGL.wrap (
      # TODO: replace with programs.emacs.extraPackages?
      (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
        epkgs.vterm
        epkgs.pdf-tools
      ])
    )
  );
in
{
  programs.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = my-emacs;
  };

  xdg = {
    enable = true;
    configFile."doom".source = config.lib.file.mkOutOfStoreSymlink ../doom;
    # Pass config from nix into doom emacs configuration
    configFile."doom_vars.el".text = ''
      (setq catppuccin-flavor '${config.catppuccin.flavor})
    '';
  };
  home.file.".authinfo.gpg".source = ../dotfiles/authinfo.gpg;
  home.shellAliases = {
    doom = "LSP_USE_PLISTS=true ~/.emacs.d/bin/doom";
    org-capture = "~/.emacs.d/bin/org-capture";
  };

  systemd.user.services."emacs" = {
    Unit.Description = "Start emacs at login";
    Install.WantedBy = [ "graphical-session.target" ];
    Service = {
      ExecStart = "${my-emacs}/bin/emacs";
    };
  };

  home.packages = with pkgs; [
    # Python
    python3Packages.debugpy # Required by dap-mode
    pyright
    python310 # Required for advanced treemacs features

    # Nix
    nil # Required by (nix +lsp)
    nixfmt-rfc-style # For emacs code formatting

    # C
    cmake-language-server

    # Rust
    rust-analyzer
    rustfmt
    clippy

    # Code Formatters
    nodePackages.prettier
    black
    shfmt

    # Language servers
    emacs-lsp-booster
    vtsls # VSCode typescript language server
    vscode-langservers-extracted
    nodePackages.dockerfile-language-server-nodejs
    python3Packages.grip # Required by grip-mode (markdown +grip)

    # Dirvish stuff
    ffmpegthumbnailer
    gnutar
    unzip
    mediainfo
    imagemagick # improves display of images in emacs

    # org-roam-graph
    graphviz

    # mermaid-mode
    nodePackages.mermaid-cli

    # Assorted Emacs dependencies
    jq # Used by restclient.el
    awscli2 # Used by s3ed
  ];
}
