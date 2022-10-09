{ config, pkgs, lib, ... }:

let settings = import ./settings.nix;
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = settings.username;
  home.homeDirectory = settings.homeDirectory;
  fonts.fontconfig.enable = true;
  targets.genericLinux.enable = true;

  imports = with lib.lists;
    [ ./dotfiles/tmux.nix ]
    ++ (optional settings.desktops.gnome.enable ./desktops/gnome);

  home.packages = with pkgs; [
    # Programming languages
    gcc # required to compile some packages, e.g. emacsqlite
    faust

    ## NodeJS
    nodejs
    yarn

    # Code Formatters (used by emacs)
    nodePackages.prettier
    shfmt
    nixfmt

    # Assorted Emacs dependencies
    python310 # Required for advanced treemacs features
    python310Packages.debugpy # Required by dap-mode
    wakatime
    nodePackages.pyright
    # Language servers
    rnix-lsp # Required by (nix +lsp)
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    nodePackages.dockerfile-language-server-nodejs
    python310Packages.grip # Required by grip-mode (markdown +grip)

    # Shell utilities
    bpytop
    curl
    jq # Format, slice and dice json on the command line. Also used by restclient.el
    ## cooler rust versions of basic GNU utilities
    fd # Better find
    ripgrep # Better grep
    exa # Better ls
    bat # Better 🐱

    # Universal dev tools
    docker
    docker-compose

    # Fonts
    # Fonts I like, in order of preference: Cascadia Code, FiraCode, Dank Mono, JetBrains Mono
    # Fonts to try: FantasqueSansMono, Inconsolata, Victor Mono
    #
    # Required by Emacs:
    cascadia-code
    (nerdfonts.override { fonts = [ "FiraCode" ]; })

    # Fun
    cowsay
    ponysay
  ];

  home.language = { base = "en_US.UTF-8"; };

  home.sessionVariables = let editor = "emacs";
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
      + "/ smona@192.168.0.198::NetBackup/${settings.hostName}";
    diskspace = "df -P -kHl";
    fonts = "fc-list";

    # Shortcuts
    c = "bat";
    g = "git";
    e = "$EDITOR";
    doom = "~/.emacs.d/bin/doom";
    hm = "home-manager";
    hms = "hm switch";
    hmn = "hm news";
    arch = "archive";
    larch = "lsarchive";
    uarch = "unarchive";

    # Navigation
    "-- -" = "cd -";

    # Better ls
    l = "ls -1a";
    ls = "exa -gFHh --git --group-directories-first";
    la = "ls -a";
    ll = "ls -l --icons"; # icons mess up alignment in grid view
    lla = "ll -a";
    lal = "ll -a";

    # Nix aliases
    nd = "nix develop -c $SHELL";
    ndp = "nix develop";
    nb = "nix build";
    ncg = "nix-collect-garbage";

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
      "doom".source = config.lib.file.mkOutOfStoreSymlink ./doom;
      "nix.conf" = {
        source = ./dotfiles/nix.conf;
        target = "nix/nix.conf";
      };
    };
  };
  home.file.".inputrc".source = ./dotfiles/inputrc;
  home.file.".p10k.zsh".source =
    config.lib.file.mkOutOfStoreSymlink ./dotfiles/p10k.zsh;

  # This is required to properly set up the login shell on some linux systems,
  # even if I don't really use bash.
  programs.bash = { enable = true; };

  programs.zsh = {
    enable = true;
    history = {
      extended = true;
      size = 1000000000; # Probably enough 😉
    };
    initExtraFirst = ''
      # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
      # Initialization code that may require console input (password prompts, [y/n]
      # confirmations, etc.) must go above this block; everything else may go below.
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
    '';
    initExtra = ''
      # Set up nix paths
      if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

      # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
      (( ! ''${+functions[p10k]} )) || p10k finalize

      # Automatically list directory contents on `cd`.
      auto-ls () {
        emulate -L zsh;
        exa --group-directories-first
      }
      chpwd_functions=( auto-ls $chpwd_functions )

      # Create a new directory and enter it
      function md() {
        mkdir -p "$@" && cd "$@"
      }

      ## Source local extra (private) settings specific to machine if it exists
      [ -f ~/.zsh.local ] && source ~/.zsh.local
    '';
    plugins = [{
      # Use zsh for nix-shell
      name = "zsh-nix-shell";
      file = "nix-shell.plugin.zsh";
      src = pkgs.fetchFromGitHub {
        owner = "chisui";
        repo = "zsh-nix-shell";
        rev = "v0.5.0";
        sha256 = "0za4aiwwrlawnia4f29msk822rj9bgcygw6a8a6iikiwzjjz0g91";
      };
    }];
  };
  programs.zsh.prezto = {
    enable = true;
    pmodules = [
      "environment"
      "terminal"
      "editor"
      "history"
      "directory"
      "spectrum"
      "utility"
      "node"
      "archive"
      "completion"
      "syntax-highlighting"
      "history-substring-search"
      "autosuggestions"
      "prompt"
    ];
    editor = {
      # Expand ... -> ../.. recursively
      dotExpansion = true;
    };
    prompt = { theme = "powerlevel10k"; };
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

  programs.emacs = {
    enable = true;
    package = pkgs.emacs28NativeComp;
    extraPackages = epkgs: [ epkgs.vterm ];
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

  programs.fzf.enable = true;
  programs.zoxide.enable = true;
  programs.less.enable = true;
  programs.direnv.enable = true;
  # https://github.com/nix-community/nix-direnv
  programs.direnv.nix-direnv.enable = true;

  programs.gh = {
    enable = true;
    settings = {
      git_protocol = "ssh";
      prompt = "enabled";
      aliases = {
        co = "pr checkout";
        pv = "pr view";
      };
    };
  };

  # Services
  services.syncthing.enable = true;
  services.keybase.enable = true;

  programs.gpg.enable = true;
  services.gpg-agent.enable = true;
  # Source: https://discourse.nixos.org/t/cant-get-gnupg-to-work-no-pinentry/15373/2
  services.gpg-agent.pinentryFlavor = "gtk2";

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
