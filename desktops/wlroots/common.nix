{ pkgs, inputs, ... }:

let
  commonOptions = (import ../common.nix);
  cmd = import ./system-commands { inherit pkgs inputs; };
in commonOptions // {
  execStart = [
    "squeekboard"
    "tablet_mode_switch"
    "gammastep-indicator -t 6500K:3800K"
    "swaync"
    "rot8"
    # Mostly just needed for 1password system authentication, so I can use the SSH agent
    # I used to use the gnome agent, but the deepin one just looks nicer, and appears to
    # be better maintained.
    "${pkgs.pantheon.pantheon-agent-polkit}/libexec/policykit-1-pantheon/io.elementary.desktop.agent-polkit"
    # TODO: handle this properly, it's specific to xps-nixos
    # It looks like xps-nixos's mic clips above 50%
    "wpctl set-volume 43 50%"
  ];
  execAlways = [ "swaybg -i ${commonOptions.backgroundImage} -m fill" ];
  keyBinds = [
    {
      primaryMod = true;
      shift = true;
      key = "e";
      command = "wlogout";
    }
    {
      primaryMod = true;
      key = "End";
      command = cmd.goodbye;
    }
    {
      secondaryMod = true;
      key = "space";
      command = "rofi -show combi -combi-modes 'drun,ssh,run' -show-icons";
    }
    {
      primaryMod = true;
      key = "period";
      command = "exec rofimoji --skin-tone light";
    }
    {
      secondaryMod = true;
      key = "tab";
      command = "rofi -show window -show-icons";
    }
    {
      primaryMod = true;
      key = "n";
      command = "swaync-client -t";
    }

    # Application shortcuts
    {
      ctrl = true;
      shift = true;
      key = "space";
      command = "1password --quick-access";
    }
    {
      primaryMod = true;
      key = "e";
      command = "nautilus";
    }
    {
      primaryMod = true;
      key = "b";
      command = "firefox";
    }
    {
      primaryMod = true;
      key = "t";
      command = "kitty";
    }
    {
      key = "Print";
      command = "grimshot --notify copy area";
    }
    {
      key = "XF86MonBrightnessUp";
      command = cmd.brighter;
      repeat = true;
    }
    {
      key = "XF86MonBrightnessDown";
      command = cmd.darker;
      repeat = true;
    }

    # Media keys
    {
      primaryMod = true;
      key = "XF86AudioLowerVolume";
      command = "${pkgs.playerctl}/bin/playerctl position 5-";
    }
    {
      primaryMod = true;
      key = "XF86AudioRaiseVolume";
      command = "${pkgs.playerctl}/bin/playerctl position 5+";
    }
    {
      secondaryMod = true;
      key = "XF86AudioLowerVolume";
      command = cmd.prev;
    }
    {
      secondaryMod = true;
      key = "XF86AudioRaiseVolume";
      command = cmd.next;
    }
    {
      ctrl = true;
      key = "XF86AudioLowerVolume";
      command = "${pkgs.playerctl}/bin/playerctl volume 0.02-";
      repeat = true;
    }
    {
      ctrl = true;
      key = "XF86AudioRaiseVolume";
      command = "${pkgs.playerctl}/bin/playerctl volume 0.02+";
      repeat = true;
    }
    {
      key = "XF86AudioLowerVolume";
      command = cmd.softer;
      repeat = true;
    }
    {
      key = "XF86AudioRaiseVolume";
      command = cmd.louder;
      repeat = true;
    }
    {
      key = "XF86AudioMute";
      command = cmd.mute;
    }
    {
      key = "XF86AudioPlay";
      command = cmd.play;
    }
    {
      key = "XF86AudioPrev";
      command = cmd.prev;
    }
    {
      key = "XF86AudioNext";
      command = cmd.next;
    }
  ];
}
