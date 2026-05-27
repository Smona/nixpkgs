# Common options across wlroots compositors, factoring in global configuration
# and per-shell or per-machine customizations

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
  # Media should stay playing when the screen locks, otherwise the idle timeout
  # can pause remotely playing media (e.g. spotify connect).
  lock = "1password --lock; ${cfg.lockCommand}";
  # Meant for when ending a session, so it does pause media.
  # note that pause will error if nothing is playing, so we can't use &&
  goodbye = "${cmd.pause} & ${lock}";
in
commonOptions
// {
  inherit lock goodbye;
  execStart = [
    "${
      inputs.roon-mpris.packages.${pkgs.system}.default
    }/bin/roon-mpris --host 192.168.0.198 --port 9330 --zone Luma"
    "${pkgs.udiskie}/bin/udiskie" # drive auto-mounting notifications via udisks2
    # Mostly just needed for 1password system authentication, so I can use the SSH agent
    # I used to use the gnome agent, but the deepin one just looks nicer, and appears to
    # be better maintained.
    "${pkgs.pantheon.pantheon-agent-polkit}/libexec/policykit-1-pantheon/io.elementary.desktop.agent-polkit"
  ]
  ++ cfg.execStart;
  execAlways = cfg.execAlways;
  keyBinds = [
    {
      primaryMod = true;
      shift = true;
      key = "e";
      command = cfg.sessionMenuCommand;
    }
    {
      primaryMod = true;
      key = "End";
      command = [ goodbye ];
    }
    {
      secondaryMod = true;
      key = "space";
      command = cfg.launcherCommand;
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
      command = cfg.notificationsCommand;
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
      key = "k";
      command = [
        "kitten"
        "quick-access-terminal"
      ];
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
      command = [ "${cmd.tao}/bin/tao" ];
    }
  ]
  ++ cfg.keyBinds;
}
