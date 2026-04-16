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
  claude-agent-acp = pkgs.buildNpmPackage (finalAttrs: {
    pname = "@zed-industries/claude-agent-acp";
    version = "0.29.0";

    src = pkgs.fetchFromGitHub {
      owner = "zed-industries";
      repo = "claude-agent-acp";
      tag = "v${finalAttrs.version}";
      hash = "sha256-L2Kq4Lk5gHqE9rpBnrKcWXMaSQqEZHtcayjGnK4fYEQ=";
    };

    npmDepsHash = "sha256-CEB4zgWC+jVpqmwbSCwE9xWoi+XiVkpNiXpDzsvbhII=";
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
    doom = "LSP_USE_PLISTS=true ~/.emacs.d/bin/doom";
    org-capture = "~/.emacs.d/bin/org-capture";
  };

  home.packages = with pkgs; [
    # Python
    python3Packages.debugpy # Required by dap-mode
    pyright
    python310 # Required for advanced treemacs features

    # Nix
    nil # Required by (nix +lsp)
    nixfmt # For emacs code formatting

    # C
    cmake-language-server

    # Code Formatters
    nodePackages.prettier
    black
    shfmt

    # Language servers
    # Overridden to enable building on darwin without building emacs from scratch.
    ((emacs-lsp-booster.override { emacs = nil; }).overrideAttrs (prev: {
      doCheck = false;
    }))
    vtsls # VSCode typescript language server
    vscode-langservers-extracted
    dockerfile-language-server
    python3Packages.grip # Required by grip-mode (markdown +grip)
    glslang # glsl-mode intellisense
    tailwindcss-language-server

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
    claude-agent-acp # Used by agent-shell
  ];
}
