{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.thunderbird = {
    enable = true;
    profiles = {
      personal = {
        isDefault = true;
        userChrome = builtins.readFile ./userChrome.css;
        userContent = builtins.readFile ./userContent.css;
      };
    };
  };
  catppuccin.thunderbird.enable = true;
}
