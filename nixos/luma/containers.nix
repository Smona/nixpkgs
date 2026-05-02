{
  config,
  lib,
  pkgs,
  ...
}:

let
  llamaSwapPort = 57680;
in
{
  config = {
    boot.enableContainers = true;

    networking.nat = {
      enable = true;
      # allow all privateNetwork containers internet access.
      internalInterfaces = [ "ve-+" ];
      # the external interface this traffic goes through.
      externalInterface = "wlp16s0";
    };

    # Llama-swap
    networking.firewall.allowedTCPPorts = [ llamaSwapPort ];
    containers.llamaswap = {
      autoStart = true;
      privateNetwork = true;
      hostAddress = "192.168.100.2";
      localAddress = "192.168.100.11";
      forwardPorts = [
        {
          protocol = "tcp";
          hostPort = llamaSwapPort;
          containerPort = llamaSwapPort;
        }
        {
          protocol = "udp";
          hostPort = llamaSwapPort;
          containerPort = llamaSwapPort;
        }
      ];

      # Provide access to the GPU
      # TODO: see if we can only mount in the specific card, and see if allowedDevices is really needed.
      allowedDevices = [
        {
          node = "/dev/dri/renderD128";
          modifier = "rwm";
        }
      ];
      bindMounts."/dev/dri" = {
        hostPath = "/dev/dri";
        isReadOnly = false;
      };

      config =
        { pkgs, ... }:
        {
          hardware.graphics.enable = true;

          services.llama-swap = {
            enable = true;
            openFirewall = true;
            # Bind to the correct interface for forwardPorts
            listenAddress = "0.0.0.0";
            port = llamaSwapPort;
            settings = {
              healthCheckTimeout = 500;
              models =
                let
                  baseArgs = "--jinja --n-gpu-layers 999 --threads 12 --port \${PORT}";
                  qwenArgs = "--ctx-size 131072 --reasoning-format deepseek";
                  qwenFilters = {
                    stripParams = "temperature, top_k, top_p, repeat_penalty, min_p, presence_penalty";
                    setParams = {
                      top_k = 20;
                      min_p = 0;
                      repetition_penalty = 1;
                    };
                    setParamsByID."\${MODEL_ID}" = {
                      chat_template_kwargs = {
                        preserve_thinking = true;
                      };
                      top_p = 0.95;
                      temperature = 0.6;
                      presence_penalty = 0.0;
                    };
                    setParamsByID."\${MODEL_ID}:instruct" = {
                      chat_template_kwargs = {
                        enable_thinking = false;
                      };
                      temperature = 0.7;
                      top_p = 0.8;
                      presence_penalty = 1.5;
                    };
                  };
                in
                {
                  "Qwen3.6-35B-A3B" = {
                    cmd = "${pkgs.llama-cpp-vulkan}/bin/llama-server --hf-repo 'unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q4_K_XL' ${qwenArgs} ${baseArgs}";
                    filters = qwenFilters;
                  };
                  "Qwen3.6-27B" = {
                    cmd = "${pkgs.llama-cpp-vulkan}/bin/llama-server --hf-repo 'unsloth/Qwen3.6-27B-GGUF:UD-Q4_K_XL' ${qwenArgs} ${baseArgs}";
                    filters = qwenFilters;
                  };
                  "WeirdCompound-v1.7-24b" = {
                    cmd = "${pkgs.llama-cpp-vulkan}/bin/llama-server --hf-repo 'mradermacher/WeirdCompound-v1.7-24b-GGUF' --ctx-size 132000 ${baseArgs}";
                  };
                  "gemma-4-26B-A4B-it" = {
                    cmd = "${pkgs.llama-cpp-vulkan}/bin/llama-server --hf-repo 'unsloth/gemma-4-26B-A4B-it-GGUF:UD-Q6_K' --ctx-size 256000 ${baseArgs}";
                    filters = {
                      setParams = {
                        temperature = 1.0;
                        top_p = 0.95;
                        top_k = 64;
                      };
                    };
                  };
                  "Qwen3.6-35B-A3B-Abliterated-Heretic" = {
                    cmd = "${pkgs.llama-cpp-vulkan}/bin/llama-server --hf-repo Youssofal/Qwen3.6-35B-A3B-Abliterated-Heretic-GGUF:Q4_K_M ${qwenArgs} ${baseArgs}";
                    filters = qwenFilters;
                  };
                };
              includeAliasesInList = true;
            };
          };
          systemd.services.llama-swap = {
            environment.XDG_CACHE_HOME = "/var/cache/llama.cpp";
            serviceConfig.CacheDirectory = "llama.cpp";
          };
          system.stateVersion = "25.01";
        };
    };
  };
}
