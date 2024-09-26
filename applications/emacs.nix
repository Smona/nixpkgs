{
  config,
  pkgs,
  inputs,
  ...
}:

let
  emacs = pkgs.emacs29-pgtk;
  nixGL = import ./nixGL.nix { inherit pkgs config; };
  my-emacs =
    (nixGL (
      # TODO: replace with programs.emacs.extraPackages?
      (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
        epkgs.vterm
        epkgs.pdf-tools
      ])
    )).overrideAttrs
      (oldAttrs: {
        # Temporarily working around this issue: https://github.com/NixOS/nixpkgs/issues/66706
        # TODO: submit upstream fix
        buildCommand =
          oldAttrs.buildCommand
          + ''
            ln -s $emacs/share/emacs $out/share/emacs
          '';
      });
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
    doom = "~/.emacs.d/bin/doom";
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
    nodePackages.typescript-language-server
    vscode-langservers-extracted
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.graphql-language-service-cli
    python310Packages.grip # Required by grip-mode (markdown +grip)

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
    wakatime
    jq # Used by restclient.el
  ];
}
