# These are all of the home-manager configurations that are shared between Linux & Darwin.
# As much stuff as possible should go in here, but some things are necessarily different between
# the two platforms.

{ pkgs, inputs, ... }:

let
  monolisa = builtins.fetchGit {
    url = "https://gitlab.com/Smona/monolisa.git";
    ref = "main";
    rev = "aa8a79e698d1cc6548e9e507f675ad35f1b9c1fc";
  };
in {
  imports = [
    inputs.dCachix.homeManagerModules.declarative-cachix
    ./applications/shell.nix
    ./applications/emacs.nix
  ];

  caches.cachix = [{
    name = "nix-community";
    sha256 = "sha256:0m6kb0a0m3pr6bbzqz54x37h5ri121sraj1idfmsrr6prknc7q3x";
  }];

  home.packages = with pkgs; [
    cachix

    ## NodeJS
    nodejs
    yarn

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
  ];

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
    generations =
      "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
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
      github = { user = "Smona"; };
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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
