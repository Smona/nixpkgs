let xdg_config_home = ~/.config;
in { config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "smona";
  home.homeDirectory = "/home/smona";
  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # Programming languages

    ## NodeJS
    nodejs
    yarn

    # Code Formatters (used by emacs)
    nodePackages.prettier
    shfmt
    nixfmt

    # Shell utilities
    bpytop
    curl
    ## cooler rust versions of basic GNU utilities
    fd # Better find
    ripgrep # Better grep
    exa # Better ls
    bat # Better 🐱

    # Universal dev tools
    hub

    # Graphical applications
    signal-desktop
    spotify

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
    RIPGREP_CONFIG_PATH = "${xdg_config_home}/.rg.conf";
    VISUAL = editor;
    EDITOR = editor;
    GIT_EDITOR = editor;
  };

  home.shellAliases = {
    # Natural language commands
    diskspace = "df -P -kHl";
    fonts = "fc-list";

    # Shortcuts
    c = "bat";
    g = "git";
    e = "$EDITOR";

    # Navigation
    "-- -" = "cd -";

    # Git aliases
    git = "hub";
    gb = "git branch";
    gcl = "git clone";
    gd = "git diff";
    gl = "git log";
    gp = "git push -u origin HEAD";
    gpu = "git pull --prune";
    gs = "git status";
    gss = "git status -s";

    # Docker
    dcu = "docker compose up";
    dcd = "docker compose down";
    dce = "docker compose exec";
    dcl = "docker compose logs -f";
    dcr = "docker compose restart";
  };

  # Dotfiles
  xdg = {
    configHome = xdg_config_home;
    enable = true;
  };
  xdg.configFile.".curlrc".source = ./curlrc;
  xdg.configFile.".rg.conf".source = ./rg.conf;
  xdg.configFile."doom".source = ./doom;

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
      if [ -e /home/smona/.nix-profile/etc/profile.d/nix.sh ]; then . /home/smona/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

      # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
      [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
      (( ! ''${+functions[p10k]} )) || p10k finalize

      # Automatically list directory contents on `cd`.
      auto-ls () {
        emulate -L zsh;
        exa --group-directories-first
      }
      chpwd_functions=( auto-ls $chpwd_functions )

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
      gruvbox
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
      let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
      let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
      set termguicolors
      let g:gruvbox_italic=1
      colorscheme gruvbox
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

  programs.firefox.enable = true;
  programs.chromium.enable = true;

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
