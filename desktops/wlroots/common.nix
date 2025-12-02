{
  pkgs,
  inputs,
  config,
  ...
}:

let
  commonOptions = (import ../common.nix);
  cmd = import ./system-commands { inherit pkgs inputs; };
  cfg = config.smona.wlroots;
  hasBuiltinDisplay = cfg.builtInDisplay != "";
in
commonOptions
// {
  execStart = [
    "squeekboard"
    "tablet_mode_switch"
    "gammastep-indicator -t 6500K:3200K -b 1.0:0.8"
    "swaync"
    "${pkgs.udiskie}/bin/udiskie" # drive auto-mounting notifications via udisks2
    # Mostly just needed for 1password system authentication, so I can use the SSH agent
    # I used to use the gnome agent, but the deepin one just looks nicer, and appears to
    # be better maintained.
    "${pkgs.pantheon.pantheon-agent-polkit}/libexec/policykit-1-pantheon/io.elementary.desktop.agent-polkit"
    # TODO: handle this properly, it's specific to xps-nixos
    # It looks like xps-nixos's mic clips above 50%
    "wpctl set-volume 43 50%"
    "firefox"
    "emacs"
    "thunderbird"
  ] ++ (pkgs.lib.lists.optional hasBuiltinDisplay "rot8");
  execAlways = [ "swaybg -i ${cfg.wallpaper} -m fill" ];
  keyBinds = [
    {
      primaryMod = true;
      shift = true;
      key = "e";
      command = [ "wlogout" ];
    }
    {
      primaryMod = true;
      key = "End";
      command = [ cmd.goodbye ];
    }
    {
      secondaryMod = true;
      key = "space";
      command = [
        "rofi"
        "-show"
        "combi"
        "-combi-modes"
        "drun,ssh,run"
        "-show-icons"
      ];
    }
    {
      primaryMod = true;
      key = "period";
      command = [
        "rofimoji"
        "--skin-tone"
        "light"
      ];
    }
    {
      primaryMod = true;
      key = "m";
      command = [
        "swaync-client"
        "-t"
      ];
    }

    # Application shortcuts
    {
      ctrl = true;
      shift = true;
      key = "space";
      command = [
        "1password"
        "--quick-access"
      ];
    }
    {
      primaryMod = true;
      key = "e";
      command = [ "nautilus" ];
    }
    {
      primaryMod = true;
      key = "b";
      command = [ "firefox" ];
    }
    {
      primaryMod = true;
      key = "k";
      command = [ "kitty" ];
    }
    {
      key = "XF86MonBrightnessUp";
      command = [ cmd.brighter ];
      repeat = true;
    }
    {
      key = "XF86MonBrightnessDown";
      command = [ cmd.darker ];
      repeat = true;
    }

    # Media keys
    {
      primaryMod = true;
      key = "XF86AudioLowerVolume";
      command = [
        "${pkgs.playerctl}/bin/playerctl"
        "position"
        "5-"
      ];
      allow_while_locked = true;
    }
    {
      primaryMod = true;
      key = "XF86AudioRaiseVolume";
      command = [
        "${pkgs.playerctl}/bin/playerctl"
        "position"
        "5+"
      ];
      allow_while_locked = true;
    }
    {
      secondaryMod = true;
      key = "XF86AudioLowerVolume";
      command = [ cmd.prev ];
      allow_while_locked = true;
    }
    {
      secondaryMod = true;
      key = "XF86AudioRaiseVolume";
      command = [ cmd.next ];
      allow_while_locked = true;
    }
    {
      ctrl = true;
      key = "XF86AudioLowerVolume";
      command = [
        "${pkgs.playerctl}/bin/playerctl"
        "volume"
        "0.02-"
      ];
      repeat = true;
      allow_while_locked = true;
    }
    {
      ctrl = true;
      key = "XF86AudioRaiseVolume";
      command = [
        "${pkgs.playerctl}/bin/playerctl"
        "volume"
        "0.02+"
      ];
      repeat = true;
      allow_while_locked = true;
    }
    {
      key = "XF86AudioLowerVolume";
      command = [ cmd.softer ];
      repeat = true;
      allow_while_locked = true;
    }
    {
      key = "XF86AudioRaiseVolume";
      command = [ cmd.louder ];
      repeat = true;
      allow_while_locked = true;
    }
    {
      key = "XF86AudioMute";
      command = [ cmd.mute ];
      allow_while_locked = true;
    }
    {
      key = "XF86AudioPlay";
      command = [ cmd.play ];
      allow_while_locked = true;
    }
    {
      key = "XF86AudioPrev";
      command = [ cmd.prev ];
      allow_while_locked = true;
    }
    {
      key = "XF86AudioNext";
      command = [ cmd.next ];
      allow_while_locked = true;
    }
    {
      primaryMod = true;
      shift = true;
      key = "a";
      command = [ cmd.tao ];
    }
  ];
}
