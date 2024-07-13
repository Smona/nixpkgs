{ config, pkgs, inputs, system, ... }:

let
  monolisa = builtins.fetchGit {
    url = "git@gitlab.com:Smona/monolisa.git";
    ref = "main";
    rev = "91cc7d681d1f7c2f0c7803f4d3fc762af98ab7c4";
  };
  nixpkgs-downgrade-gpg =
    import inputs.nixpkgs-downgrade-gpg { inherit system; };
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.homeDirectory = "/home/${config.home.username}";
  fonts.fontconfig.enable = true;
  targets.genericLinux.enable = true;

  imports = [
    ./desktops/gnome
    ./desktops/wlroots
    ./applications/tmux.nix
    ./applications/emacs.nix
    ./applications/shell.nix
    ./peripherals/logitech.nix
  ];

  home.packages = with pkgs; [
    # Programming languages
    gcc # required to compile some packages, e.g. emacsqlite
    faust

    ## Python
    python310 # Required for advanced treemacs features

    ## NodeJS
    nodejs
    yarn

    ## Haskell
    ghc
    haskell-language-server # Required for doom (haskell +lsp) module

    ## Rust
    cargo
    cargo-watch
    rustc

    # system tools
    dmidecode
    wireplumber
    appimage-run
    dig
    dua # disk usage analyzer

    # Universal dev tools
    docker
    docker-compose
    vulnix
    # Cloud administration
    terraform
    awscli2

    # Fonts
    # Fonts I like, in order of preference: MonoLisa, Cascadia Code, FiraCode, Dank Mono, JetBrains Mono
    # Fonts to try: FantasqueSansMono, Inconsolata, Victor Mono
    cascadia-code
    # NerdFontsSymbolsOnly required by Emacs
    (nerdfonts.override { fonts = [ "CascadiaCode" "NerdFontsSymbolsOnly" ]; })
    noto-fonts-emoji # Required by Emacs
    source-serif # Required by Emacs
    source-sans
    rounded-mgenplus # Japanese font support
    (callPackage monolisa { })

    # Fun
    cowsay
    ponysay
    pridecat
    nyancat
    blahaj
  ];

  home.language = { base = "en_US.UTF-8"; };

  home.sessionVariables = let editor = "vim";
  in {
    RIPGREP_CONFIG_PATH = "${./dotfiles/rg.conf}";
    VISUAL = editor;
    EDITOR = editor;
    GIT_EDITOR = editor;
  };

  home.shellAliases = {
    # Natural language commands
    backup = "sudo rsync -av --delete "
      + "-e 'ssh -oKexAlgorithms=+diffie-hellman-group1-sha1 -oHostKeyAlgorithms=+ssh-rsa' "
      + "--exclude-from='${./dotfiles/rsync-ignore.txt}' "
      + "/ smona@192.168.0.198::NetBackup/$(hostname)";
    diskspace = "df -P -kHl";
    fonts = "fc-list";
    bios = "sudo dmidecode -t bios";
    fucking = "sudo";

    # Shortcuts
    c = "bat";
    cat = "bat";
    g = "git";
    e = "$EDITOR";
    upgrade = "sudo nixos-rebuild --flake ~/.config/nixpkgs";
    hm = "home-manager --flake ~/.config/nixpkgs";
    hms = "hm switch --max-jobs 4";
    hmn = "hm news";
    arch = "archive";
    larch = "lsarchive";
    uarch = "unarchive";

    # Nix aliases
    nd = "nix develop -c $SHELL";
    ndp = "nix develop";
    nb = "nix build";
    ncg = "sudo nix-collect-garbage";

    # Git aliases
    gb = "git branch";
    gcl = "git clone";
    gd = "git diff";
    gl = "git log";
    gp = "git push -u origin HEAD";
    gpu = "git pull --prune";
    gs = "git status";
    gss = "git status -s";
    gst = "git stash";
    gstp = "git stash pop";
    gstl = "git stash list";

    # Docker aliases
    dcu = "docker compose up";
    dcd = "docker compose down";
    dce = "docker compose exec";
    dcl = "docker compose logs -f";
    dcr = "docker compose restart";
  };

  # Dotfiles
  xdg = {
    enable = true;
    configFile = {
      ".curlrc".source = ./dotfiles/curlrc;
      "nix.conf" = {
        source = ./dotfiles/nix.conf;
        target = "nix/nix.conf";
      };
    };
  };
  home.file.".inputrc".source = ./dotfiles/inputrc;

  programs.ssh = {
    enable = true;
    # Set up the 1password SSH agent
    extraConfig = "IdentityAgent ~/.1password/agent.sock";
    includes = [ "~/.ssh/config.local" ];
  };

  programs.git = {
    enable = true;
    userName = "Mel Bourgeois";
    userEmail = "mason.bourgeois@gmail.com";
    signing = {
      signByDefault = true;
      key = "290FCF081AEDB3EC";
    };
    includes = [
      # Use separate file for username / github token / etc
      { path = "~/.gitconfig.local"; }
    ];
    delta = {
      enable = true;
      # NB: adding line-numbers feature breaks magit-delta
      # https://github.com/dandavison/magit-delta/issues/13
      options = {
        features = "side-by-side decorations";
        syntax-theme = "Sublime Snazzy";
        hyperlinks = true;
      };
    };
    extraConfig = {
      core = { autocrlf = "input"; };
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
      fetch = { prune = true; };
      rebase = { autoStash = true; };
      merge = { autoStash = true; };
      # Correct typos
      help = { autocorrect = 1; };
      # Use different colors for moved code vs additions/deletions.
      diff = { colorMoved = "zebra"; };
      # Forges
      github = { user = "Smona"; };
    };
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
      vim-wakatime
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
  services.keybase.enable = true;

  programs.gpg.enable = true;
  # TODO: remove this and the input once the referenced issue is fixed
  programs.gpg.package = nixpkgs-downgrade-gpg.gnupg;

  services.gpg-agent.enable = true;
  # Source: https://discourse.nixos.org/t/cant-get-gnupg-to-work-no-pinentry/15373/2
  services.gpg-agent.pinentryPackage = pkgs.pinentry-gnome;

  # Automatically start/stop/restart services when their configuration changes
  systemd.user.startServices = "sd-switch";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
