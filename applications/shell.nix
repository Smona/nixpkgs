{
  pkgs,
  config,
  lib,
  ...
}:
let
  # Aliases that only work and/or are needed outside of nushell:
  posixAliases = {
    l = "ls -1a";
    ls = "eza -gHh --git --group-directories-first";
    ll = "ls -l --icons"; # icons mess up alignment in grid view
    nd = "nix develop -c $SHELL";
    update = "cd ~/.config/nixpkgs && nix flake lock --update-input nixpkgs --update-input home-manager --update-input spicetify-nix && upgrade switch";
  };
in
{
  programs.fzf.enable = true;
  programs.zoxide.enable = true;
  programs.less.enable = true;

  home.packages = with pkgs; [
    # Shell utilities
    curl
    jq # Format, slice and dice json on the command line.
    dig
    whois

    # Universal dev tools
    git
    git-filter-repo
    docker
    docker-compose
    kubectl

    ## cooler rust versions of basic GNU utilities
    fd # Better find
    ripgrep # Better grep
    eza # Better ls
    dua # disk usage analyzer

    # nix-specific utilities
    nix-tree

    # Fun
    cowsay
    ponysay
    pridecat
    nyancat
    blahaj
  ];

  home.shellAliases = {
    # Natural language commands
    backup =
      "sudo rsync -av --delete "
      + "-e 'ssh -oKexAlgorithms=+diffie-hellman-group1-sha1 -oHostKeyAlgorithms=+ssh-rsa' "
      + "--exclude-from='${../dotfiles/rsync-ignore.txt}' "
      + "/ smona@192.168.0.198::NetBackup/$(hostname)";
    diskspace = "df -P -kHl";
    fonts = "fc-list";
    bios = "sudo dmidecode -t bios";
    fucking = "sudo";
    # Count lines in a git repo
    lines = "git ls-files | xargs wc -l | tail -n1";

    # Shortcuts
    c = "bat";
    cat = "bat";
    g = "git";
    upgrade = if pkgs.stdenv.isDarwin then "nh darwin" else "nh os";
    generations = "sudo nix-env --list-generations --profile /nix/var/nix/profiles/system";
    hm = "home-manager --flake ~/.config/nixpkgs";
    hms = "hm switch --max-jobs 4";
    hmn = "hm news";
    arch = "archive";
    larch = "lsarchive";
    uarch = "unarchive";

    # Nix aliases
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

    # Better ls
    la = "ls -a";
    lla = "ll -a";
    lal = "ll -a";
    # Better terraform
    tf = "terraform";
    tfa = "tf apply";
    tfp = "tf plan";
    tfi = "tf init";
    tfd = "tf destroy";
    # Better kubernetes
    k = "kubectl";
  };
  programs.bash.shellAliases = posixAliases;
  programs.zsh.shellAliases = posixAliases;
  programs.nushell.shellAliases = {
    ll = "ls -l";
    fg = "job unfreeze";
  };

  # Better 🐱
  programs.bat.enable = true;
  # System monitor
  programs.btop.enable = true;

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

  home.file.".p10k.zsh".source = config.lib.file.mkOutOfStoreSymlink ../dotfiles/p10k.zsh;

  # This is required to properly set up the login shell on some linux systems,
  # even if I don't really use bash.
  programs.bash.enable = true;

  programs.nushell = {
    enable = true;
    configFile.source = ./config.nu;
  };

  programs.zsh = {
    enable = true;
    history = {
      extended = true;
      size = 1000000000; # Probably enough 😉
    };
    initContent = lib.mkMerge [
      (lib.mkBefore ''
        # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
        # Initialization code that may require console input (password prompts, [y/n]
        # confirmations, etc.) must go above this block; everything else may go below.
        if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi
      '')
      ''
        export SSH_AUTH_SOCK=${config.home.homeDirectory}/.1password/agent.sock

        # Set up nix paths
        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

        # To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
        [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
        (( ! ''${+functions[p10k]} )) || p10k finalize

        # Automatically list directory contents on `cd`.
        auto-ls () {
          emulate -L zsh;
          eza --group-directories-first
        }
        chpwd_functions=( auto-ls $chpwd_functions )

        # Create a new directory and enter it
        function md() {
          mkdir -p "$@" && cd "$@"
        }

        # Prevent extended glob from breaking nix flake URLs
        alias nix="noglob nix"

        ## Source local extra (private) settings specific to machine if it exists
        [ -f ~/.zsh.local ] && source ~/.zsh.local
      ''
    ];
    plugins = [
      {
        # Use zsh for nix-shell
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.5.0";
          sha256 = "0za4aiwwrlawnia4f29msk822rj9bgcygw6a8a6iikiwzjjz0g91";
        };
      }
    ];
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
    prompt.theme = "powerlevel10k";
  };
}
