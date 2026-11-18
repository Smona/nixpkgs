# NixOS module for hosts that run a local LLM inference server
# (llama-swap in a NixOS container with GPU passthrough).
{ ... }:
{
  flake.nixosModules.llm-server =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      llamaSwapPort = 57680;
      openWebuiPort = 8082;
    in
    {
      config = {
        boot.enableContainers = true;

        networking.nat = {
          enable = true;
          # allow all privateNetwork containers internet access.
          internalInterfaces = [ "ve-+" ];
          # the external interface this traffic goes through.
          # changed from "wlp16s0" when i switched to iwd
          externalInterface = "wlan0";
        };

        # Llama-swap + Open-WebUI (colocated so open-webui can reach llama-swap on localhost)
        networking.firewall.allowedTCPPorts = [
          llamaSwapPort
          openWebuiPort
        ];
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
            {
              protocol = "tcp";
              hostPort = openWebuiPort;
              containerPort = openWebuiPort;
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
              # open-webui is unfree; containers don't inherit the host's nixpkgs config.
              nixpkgs.config.allowUnfree = true;

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
                      llama-cpp-with-repo = "${pkgs.llama-cpp-vulkan}/bin/llama-server --hf-repo";
                      # using speculative decoding to try and speed up models
                      # --no-mmap strictly improves model loading speed, and has no negative effect on
                      #   RAM usage because we are always aiming to fit everything on the GPU.
                      #   WIP: disabling no-mmap since i actually do want to push it to the limit ^ ^
                      baseArgs = "--verbosity 4 --jinja --n-gpu-layers 999 --threads 12 --kv-unified --port \${PORT}";
                      # conservatively quantize the KV cache
                      quantizeArgs = "-ctk q8_0 -ctv q8_0";
                      # q8_0 KV quantization unlocks a ton more context, and doesn't seem to have downsides on dense models tested so far.
                      # My card is compute-bound with dense models, so parallel execution doesn't really work.
                      # TODO: figure out how to avoid invalidating entire context with pi-observational-memory
                      denseArgs = "--parallel 1 ${quantizeArgs} ${baseArgs}";
                      # do not use KV quantization on moe models, it slows ones tested down by ~4x
                      # https://www.reddit.com/r/LocalLLaMA/comments/1ng0fmv/comment/ne0gwx3/
                      # --parallel 2 to free up a little bit of VRAM for context, but keep unified KV
                      # My card is VRAM-bound with MoE models, so they can handle some parallelism.
                      # TODO: confirm that models can handle 2 parallel queries with decent performance
                      moeArgs = "--parallel 2 ${baseArgs}";
                      # --no-mmproj disables image processing to free up VRAM.
                      qwenArgs = "--no-mmproj --reasoning-format deepseek";
                      # Enable multi-token prediction (requires a model with MTP support).
                      # spec-draft-n-max not optimized, just using unsloth's recommendation.
                      # it seems like dense models benefit more from mtp
                      mtpArgs = "--spec-type draft-mtp --spec-draft-n-max 2";
                      nonMtpArgs = "--spec-default";
                      gemmaFilters = {
                        setParams = {
                          chat_template_kwargs = {
                            enable_thinking = true;
                          };
                          temperature = 1.0;
                          top_p = 0.95;
                          top_k = 64;
                        };
                      };
                      qwenFilters =
                        {
                          thinkingTemp ? 0.6,
                        }:
                        {
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
                            temperature = thinkingTemp;
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
                      # This model spills out of VRAM a bit, but seems to be able to tolerate <1GB of overflow without slowdown.
                      "Qwen3.6-35B-A3B" = {
                        cmd = "${llama-cpp-with-repo} unsloth/Qwen3.6-35B-A3B-GGUF:UD-Q4_K_XL --ctx-size 32000 ${nonMtpArgs} ${qwenArgs} ${moeArgs}";
                        filters = qwenFilters { };
                      };
                      "Qwen3.8-27B" = {
                        cmd = "${llama-cpp-with-repo} unsloth/Qwen3.8-27B-GGUF:UD-Q4_K_XL --ctx-size 100000 ${mtpArgs} ${qwenArgs} ${denseArgs}";
                        filters = qwenFilters { thinkingTemp = 1.0; };
                      };
                      "Cydonia-24B-v4.3" = {
                        cmd = "${llama-cpp-with-repo} TheDrummer/Cydonia-24B-v4.3-GGUF:Q6_K --ctx-size 36000 ${nonMtpArgs} ${denseArgs}";
                      };
                      # Q5_K_S runs super fast, but writing quality is significantly degraded compared to the XL.
                      # it seems XL variants can really be much better.
                      "gemma-4-26B-A4B-it" = {
                        cmd = "${llama-cpp-with-repo} unsloth/gemma-4-26B-A4B-it-GGUF:UD-Q5_K_XL --no-mmproj --ctx-size 90000 --no-warmup ${nonMtpArgs} ${moeArgs}";
                        filters = gemmaFilters;
                      };
                      "gemma-4-31b" = {
                        cmd = "${llama-cpp-with-repo} unsloth/gemma-4-26B-A4B-it-GGUF:UD-Q6_K_XL --no-mmproj --ctx-size 60000 --no-warmup ${nonMtpArgs} ${denseArgs}";
                        filters = gemmaFilters;
                      };
                      "gemma-4-Gembrain-31b" = {
                        cmd = "${llama-cpp-with-repo} mradermacher/Gemma-4-Gembrain-31B-i1-GGUF:Q5_K_S --no-mmproj --ctx-size 40000 ${nonMtpArgs} ${denseArgs}";
                        filters = gemmaFilters;
                      };
                      "Gemsicle-31b" = {
                        cmd = "${llama-cpp-with-repo} mradermacher/Gemma-4-Gemsicle-31B-i1-GGUF:Q4_K_M --no-mmproj --ctx-size 64000 ${nonMtpArgs} ${denseArgs}";
                        filters = gemmaFilters;
                      };
                      "G4-MeroMero-31B" = {
                        cmd = "${llama-cpp-with-repo} zerofata/G4-MeroMero-31B-gguf:Q5_K_M --no-mmproj --ctx-size 60000 --no-warmup ${nonMtpArgs} ${denseArgs}";
                        filters = gemmaFilters;
                      };
                      "G4-MeroMero-26B-A4B" = {
                        cmd = "${llama-cpp-with-repo} zerofata/G4-MeroMero-26B-A4B-gguf:Q6_K --ctx-size 64000 ${nonMtpArgs} ${moeArgs}";
                      };
                      "Skyfall-31B-v.2" = {
                        cmd = "${llama-cpp-with-repo} bartowski/TheDrummer_Skyfall-31B-v4.2-GGUF:Q5_K_M --no-mmproj --ctx-size 90000 ${nonMtpArgs} ${denseArgs}";
                      };
                      # TODO: find better-optimized variant, or pick a better abliterated model
                      "Qwen3.6-35B-A3B-Abliterated-Heretic" = {
                        cmd = "${llama-cpp-with-repo} Youssofal/Qwen3.6-35B-A3B-Abliterated-Heretic-GGUF:Q4_K_M --ctx-size 64000 ${nonMtpArgs} ${qwenArgs} ${moeArgs}";
                      };
                    };
                  # Enable selecting instruct variants in model list.
                  includeAliasesInList = true;
                };
              };

              services.open-webui = {
                enable = true;
                openFirewall = true;
                # Bind on the container's interfaces for forwardPorts to reach it.
                host = "0.0.0.0";
                port = openWebuiPort;
                environment = {
                  # Talk to the llama-swap instance running in this same container.
                  ENABLE_OLLAMA_API = "False";
                  OPENAI_API_BASE_URL = "http://127.0.0.1:${toString llamaSwapPort}/v1";
                  OPENAI_API_KEY = "none";
                };
              };

              system.stateVersion = "25.01";
            };
        };
      };
    };
}
