# Package little-coder and pi-acp, expose via home-manager.
{ self, ... }:
{
  perSystem =
    { pkgs, self', ... }:
    {
      # TODO: re-evaluate if it's fixed
      # packages.little-coder = pkgs.buildNpmPackage rec {
      #   pname = "little-coder";
      #   version = "1.8.1";

      #   src = pkgs.fetchFromGitHub {
      #     owner = "itayinbarr";
      #     repo = pname;
      #     tag = "v${version}";
      #     hash = "sha256-8MY8BLimdZTFjPcXIdeAF484g1T0FVvjehrRtd+PuYM=";
      #   };
      #   npmDepsHash = "sha256-TmjIE/Iifj3rt/oVgfDxvIKoeVaOhfrlczQNA6UprA8=";
      #   dontNpmBuild = true;
      # };

      packages.pi-acp = pkgs.buildNpmPackage rec {
        pname = "pi-acp";
        version = "0.0.34";

        src = pkgs.fetchFromGitHub {
          owner = "svkozak";
          repo = "pi-acp";
          tag = "v${version}";
          hash = "sha256-QRwxOtTZOY+Np3PkAoy2o2PrUzEqjItM/372sCPlSMo=";
        };
        npmDepsHash = "sha256-BvLNtFfp1cMVjzWcMRSdhTqiJrTfbFoUbWkkPW9200o=";
      };
    };

  # TODO: add keybind for vterm-send-escape in vterm buffers
  flake.homeModules.pi =
    { pkgs, config, ... }:
    let
      # TODO: install extensions declaratively
      pi-with-extensions = (
        pkgs.symlinkJoin {
          name = "pi-coding-agent";
          buildInputs = [ pkgs.makeWrapper ];
          paths = [ pkgs.pi-coding-agent ];
          postBuild = ''
            wrapProgram $out/bin/pi \
              --set NPM_CONFIG_PREFIX ${config.home.homeDirectory}/.pi/npm/ \
              --prefix PATH : ${
                pkgs.lib.makeBinPath [
                  pkgs.nodejs_latest
                ]
              }
          '';
        }
      );

      # Hardened sandbox to restrict pi agent to what it needs
      # TODO: install command-filtering extension to ask for sudo and other harmful commands
      safe-pi = pkgs.writeShellApplication {
        name = "pi";
        runtimeInputs = with pkgs; [
          bubblewrap
          util-linux
        ];
        text = ''
          HERE="$(realpath .)"
          if [ "$HERE" = "/" ] || [ "$HERE" = "$HOME" ]; then
              echo "refusing to sandbox $HERE (too broad)"; exit 1
          fi

          # Forward env vars typically set by `nix develop` / `nix-shell`
          # so toolchains and build flags survive --clearenv.
          forward=(
            IN_NIX_SHELL
            NIX_PATH NIX_STORE NIX_BUILD_TOP NIX_BUILD_CORES
            NIX_CFLAGS_COMPILE NIX_LDFLAGS
            PKG_CONFIG_PATH CMAKE_PREFIX_PATH
            LD_LIBRARY_PATH LIBRARY_PATH
            XDG_DATA_DIRS LOCALE_ARCHIVE SOURCE_DATE_EPOCH
            CC CXX LD CFLAGS CXXFLAGS LDFLAGS
          )
          nix_env_args=()
          for v in "''${forward[@]}"; do
              val="''${!v:-}"
              [ -n "$val" ] && nix_env_args+=(--setenv "$v" "$val")
          done

          echo "Entering sandbox for $HERE"
          exec bwrap \
              --ro-bind /usr /usr \
              --ro-bind /bin /bin --ro-bind-try /sbin /sbin \
              --ro-bind /lib /lib --ro-bind /lib64 /lib64 \
              --ro-bind /etc /etc \
              --ro-bind-try /nix /nix \
              --ro-bind /run /run \
              --proc /proc \
              --dev /dev \
              --tmpfs /tmp \
              --bind "$HOME/.pi" "$HOME/.pi" \
              --bind "$HERE" "$HERE" \
              --chdir "$HERE" \
              --unshare-all --share-net \
              --new-session \
              --die-with-parent \
              --cap-drop ALL \
              --clearenv \
              --setenv HOME "$HOME" \
              --setenv PATH "$PATH" \
              --setenv TERM "$TERM" \
              --setenv PS1 "sandbox\$ " \
              "''${nix_env_args[@]}" \
            ${pi-with-extensions}/bin/pi "$@"
        '';
      };
    in
    {
      home.packages = [
        safe-pi
        self.packages.${pkgs.stdenv.hostPlatform.system}.pi-acp
        pkgs.rtk # for pi-rtk-optimizer, reduces tokens from bash commands
      ];

      # TODO: explain the sandbox the agent is running in to it in the system prompt.
      home.file.".pi/agent/models.json".text = ''
        {
          "providers": {
            "llamacpp": {
              "api": "openai-completions",
              "baseUrl": "http://192.168.0.25:57680/v1",
              "apiKey": "none",
              "models": [
                {
                  "id": "Qwen3.6-35B-A3B",
                  "name": "Qwen3.6",
                  "reasoning": true,
                  "contextWindow": 85000
                },
                {
                  "id": "Qwen3.6-27B",
                  "name": "Qwen3.6 Dense",
                  "reasoning": true,
                  "contextWindow": 70000
                },
                {
                  "id": "Qwen3.8-27B",
                  "name": "Qwen3.8 Dense",
                  "reasoning": true,
                  "contextWindow": 100000
                },
                {
                  "id": "gemma-4-26B-A4B-it",
                  "name": "Gemma 4",
                  "reasoning": true,
                  "contextWindow": 64000
                },
                {
                  "id": "gemma-4-31b",
                  "name": "Gemma 4 31b",
                  "reasoning": true,
                  "contextWindow": 60000
                }
              ]
            }
          }
        }
      '';
    };
}
