{ pkgs, ... }:

{
  config = {
    home.packages = with pkgs; [
      opencode
      mcp-nixos
    ];
    xdg.configFile."opencode/opencode.json".text = ''
      {
          "$schema": "https://opencode.ai/config.json",
          "permission": {
              "bash": "ask",
              "edit": "ask"
          },
          "mcp": {
              "mcp-nixos": {
                  "type": "local",
                  "command": ["mcp-nixos"],
                  "enabled": true
              }
          },
          "provider": {
              "llama-server": {
                  "npm": "@ai-sdk/openai-compatible",
                  "name": "llama.cpp (local)",
                  "options": {
                      "baseURL": "http://192.168.0.25:57680/v1"
                  },
                  "models": {
                      "Qwen3.6-35B-A3B": {
                          "name": "Qwen3.6",
                          "options": {
                              "chat_template_kwargs": {
                                  "preserve_thinking": true
                              }
                          }
                      },
                  }
              }
          }
      }
    '';
  };
}
