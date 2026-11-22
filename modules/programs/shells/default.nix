{ ... }:
{
  den.aspects.shells.includes = [
    (den.batteries.unfree [ "pridecat" ])
  ];

  den.aspects.shells.homeManager =
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
        (pkgs.writeShellScriptBin "upgrade" (
          if pkgs.stdenv.hostPlatform.isDarwin then
            ''
              nh darwin switch --ask "$@"
            ''
          else
            ''
              set -euo pipefail

              # 1. Build the configuration without activating it (creates ./result)
              nh os build --diff=never "$@"
              echo

              # 2. Compare booted initrd (and kernel/modules) with the newly built derivation
              booted_boot_files="$(readlink -f /run/booted-system/{initrd,kernel,kernel-modules})"
              built_boot_files="$(readlink -f ./result/{initrd,kernel,kernel-modules})"

              # 3. Choose 'boot' if early-boot components changed, otherwise 'switch'
              # Capture the exit code so a rejected change (exit 1) doesn't abort
              # under `set -e` before cleanup, and is preserved when the script ends.
              status=0
              needs_reboot=0
              if [ "$booted_boot_files" != "$built_boot_files" ]; then
                echo "Kernel or initrd changed; setting as next boot target..."
                echo
                nh os boot --ask --quiet --no-rom || status=$?
                if [ "$status" -eq 0 ]; then
                  needs_reboot=1
                fi
              else
                echo "No kernel or initrd changes detected. Switching to the new configuration..."
                echo
                nh os switch --ask --quiet --no-rom || status=$?
              fi

              # Clean up the temporary result symlink
              rm -f ./result

              # Offer to reboot now that the new boot target is set and cleanup is done.
              # Default is yes (Y/n); falls back to no on EOF/non-interactive input.
              if [ "$needs_reboot" -eq 1 ]; then
                echo
                read -r -p "Reboot now to apply the new configuration? [Y/n] " reply || reply="n"
                # Only an explicit y/yes (or Enter) reboots, so stray input never
                # reboots by accident. Strip whitespace, match case-insensitively.
                reply="''${reply//[[:space:]]/}"
                shopt -s nocasematch
                case "$reply" in
                  "" | y | yes)
                    echo "Rebooting..."
                    sudo reboot
                    ;;
                  *)
                    echo "Not rebooting. Please reboot soon!"
                    ;;
                esac
                shopt -u nocasematch
              fi

              exit "$status"
            ''
        ))
        (pkgs.writeShellScriptBin "update" ''
          upgrade --update
        '')
        (pkgs.writeShellScriptBin "clean" ''
          nh clean all --keep 20 --keep-since 4w --ask --no-direnv
        '')

        # Fun
        cowsay
        ponysay
        pridecat
        nyancat
        blahaj
      ];

      home.sessionVariables = {
        RIPGREP_CONFIG_PATH = "${./rg.conf}";
      };

      # Dotfiles
      xdg = {
        enable = true;
        configFile = {
          ".curlrc".source = ./curlrc;
        };
      };
      home.file.".inputrc".source = ./inputrc;

      home.shellAliases = {
        # Natural language commands
        backup =
          "sudo rsync -av --delete "
          + "-e 'ssh -oKexAlgorithms=+diffie-hellman-group1-sha1 -oHostKeyAlgorithms=+ssh-rsa' "
          + "--exclude-from='${./rsync-ignore.txt}' "
          + "/ smona@192.168.0.198::NetBackup/$(hostname)";
        diskspace = "df -P -kHl";
        fonts = "fc-list";
        bios = "sudo dmidecode -t bios";
        fucking = "sudo";
        # Count lines in a git repo by language
        lines = "eza --code";

        # Shortcuts
        c = "bat";
        cat = "bat";
        g = "git";
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
        unixstamp = "into datetime -f %s";
      };

      # Better 🐱
      programs.bat.enable = true;
      # System monitor
      programs.btop = {
        enable = true;
        settings = lib.mkIf (config.smona.desktop.shell == "noctalia") {
          color_theme = "noctalia";
        };
      };
      catppuccin.btop.enable = config.smona.desktop.shell != "noctalia";

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

      # This is required to properly set up the login shell on some linux systems,
      # even if I don't really use bash.
      programs.bash.enable = true;

      programs.nushell = {
        enable = true;
        configFile.source = ./config.nu;
      };
      programs.oh-my-posh = {
        enable = true;
        settings = builtins.fromTOML (builtins.readFile ./oh-my-posh.config.toml);
      };

      programs.zsh = {
        enable = true;
        # Use XDG config dir for zsh dotfiles
        dotDir = "${config.xdg.configHome}/zsh";
        history = {
          extended = true;
          size = 1000000000; # Probably enough 😉
        };
        initContent = ''
          export SSH_AUTH_SOCK=${config.home.homeDirectory}/.1password/agent.sock

          # Set up nix paths
          if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

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
        '';
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
      };
    };
}
