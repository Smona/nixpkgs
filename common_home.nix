# These are all of the home-manager configurations that are shared between Linux & Darwin.
# As much stuff as possible should go in here, but some things are necessarily different between
# the two platforms.

{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

let
  monolisa = pkgs.callPackage (import ./pkgs/monolisa.nix) { };
in
{
  imports = [
    inputs.dCachix.homeManagerModules.declarative-cachix
    inputs.catppuccin.homeModules.catppuccin
    ./applications/shell.nix
    ./applications/tmux.nix
    ./applications/emacs.nix
    ./applications/common_gui.nix
  ];

  options = with lib; {
    pkgsCompat = mkOption {
      type = types.pkgs;
      default = pkgs;
      description = ''
        A version of nixpkgs which lags behind the main package set, to preserve
        compatibility with non-NixOS installs on distributions that have slower
        release cycles (e.g. Ubuntu).

        This is especially necessary for packages which integrate closely with the
        system-supplied desktop environment (e.g. gnome extensions), but can come
        up in other cases as well.
      '';
    };
  };

  config = {
    caches.cachix = [
      {
        name = "nix-community";
        sha256 = "sha256:0m6kb0a0m3pr6bbzqz54x37h5ri121sraj1idfmsrr6prknc7q3x";
      }
    ];

    catppuccin.enable = true;
    catppuccin.flavor = "mocha";

    # Configuration of the nix CLI
    # https://nixos.org/manual/nix/stable/command-ref/conf-file.html
    nix.package = lib.mkDefault pkgs.nix;
    nix.settings = {
      # Massively reduce disk usage by hard linking any duplicate files
      # to a single location on disk.
      # FIXME: only enable this outside of nix-darwin & nixos.
      # broken on darwin: https://github.com/NixOS/nix/issues/7273
      # auto-optimise-store = true;
      # Enable nix command and nix flakes
      experimental-features = "nix-command flakes";
      # Always show failure traces
      show-trace = true;
    };

    home.language.base = "en_US.UTF-8";

    home.packages = with pkgs; [
      cachix

      ## NodeJS
      nodejs
      yarn
      nodePackages.prettier

      # Fonts
      # Fonts I like, in order of preference: MonoLisa, Cascadia Code, FiraCode, Dank Mono, JetBrains Mono
      # Fonts to try: FantasqueSansMono, Inconsolata, Victor Mono
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

    home.sessionVariables =
      let
        editor = "vim";
      in
      {
        RIPGREP_CONFIG_PATH = "${./dotfiles/rg.conf}";
        VISUAL = editor;
        EDITOR = editor;
        GIT_EDITOR = editor;
      };

    # Enable nix CLI helper
    programs.nh = {
      enable = true;
      flake = "$HOME/.config/nixpkgs";
    };

    programs.git = {
      enable = true;
      signing = {
        signByDefault = true;
        key = "290FCF081AEDB3EC";
      };
      includes = [
        # Use separate file for username / github token / etc
        { path = "~/.gitconfig.local"; }
      ];
      settings = {
        user = {
          name = "Mel Bourgeois";
          email = "mason.bourgeois@gmail.com";
        };
        core.autocrlf = "input";
        init.defaultBranch = "main";
        pull.rebase = true;
        fetch.prune = true;
        rebase.autoStash = true;
        merge.autoStash = true;
        # Correct typos
        help.autocorrect = 1;
        diff = {
          # Use different colors for moved code vs additions/deletions.
          colorMoved = "zebra";
          # Diff bun lockfiles legibly, according to docs. Fun setup step! 🙄
          lockb = {
            textconv = "bun";
            binary = true;
          };
        };
        # Forges
        github.user = "Smona";
        gitlab.user = "Smona";
      };
      attributes = [ "*.lockb binary diff=lockb" ];
      ignores = [
        ".gitconfig.local" # Local config file
        "private" # General-purpose private directories
        # Folder view configuration files
        ".DS_Store"
        "Desktop.ini"
        # Thumbnail cache files
        "._*"
        "*~"
        "Thumbs.db"
        # Files that might appear on external disks
        ".Spotlight-V100"
        ".Trashes"
        # Compiled Python files
        "*.pyc"
        # npm & bower
        "bower_components"
        "node_modules"
        "npm-debug.log"
        "yarn-error.log"
        # IDEs stuff
        ".idea"
        ".log"
      ];
    };
    # Fancier git diffs
    programs.delta = {
      enable = true;
      enableGitIntegration = true;
      # NB: adding line-numbers feature breaks magit-delta
      # https://github.com/dandavison/magit-delta/issues/13
      options = {
        features = lib.mkForce "side-by-side decorations catppuccin-${config.catppuccin.flavor}";
        hyperlinks = true;
      };
    };

    # Dotfiles
    xdg = {
      enable = true;
      configFile = {
        ".curlrc".source = ./dotfiles/curlrc;
      };
    };
    home.file.".inputrc".source = ./dotfiles/inputrc;

    programs.ssh = {
      enable = true;
      includes = [ "~/.ssh/config.local" ];
    };

    programs.vim = {
      enable = true;
      settings = {
        # Enable mouse navigation
        mouse = "a";
        # tabs are spaces
        expandtab = true;
        # number of spaces when reindinting
        shiftwidth = 2;
        # number of visual spaces per TAB
        tabstop = 2;
        ignorecase = true;
        smartcase = true;
        # Allow hiding unsaved buffers
        hidden = true;
      };
      plugins = with pkgs.vimPlugins; [
        vim-commentary
        vim-repeat
        vim-surround
      ];
      extraConfig = ''
        set nocompatible              " be iMproved
        :let mapleader = ","

        set splitbelow
        set splitright

        set encoding=utf-8
        set autoindent

        " Indent Settings
        set softtabstop=2       " number of spaces in tab when editing

        " Search Settings
        set incsearch           " search as characters are entered
        set hlsearch            " highlight matches
        " turn off search highlight
        nnoremap <leader><space> :nohlsearch<CR>

        inoremap <C-c> <Esc>

        set t_Co=256            " Use 256 colours always

        " Theme choice, with optimizations for gruvbox
        set termguicolors
        set background=dark

        " Use hybrid line numbers while editing file, and absolute otherwise
        " NOTE: disabled to improve performance
        set number " relativenumber
        " augroup numbertoggle
        "   autocmd!
        "   autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
        "   autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
        " augroup END
      '';
    };

    # Services
    services.syncthing.enable = true;

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;
  };
}
