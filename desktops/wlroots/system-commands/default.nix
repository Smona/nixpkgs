{ pkgs, inputs, ... }:

let
  maxVolume = "1.2";
  commonOptions = import ../../common.nix;
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  wpctl = "${pkgs.wireplumber}/bin/wpctl";
  hyprctl = "${inputs.hyprland.packages.x86_64-linux.hyprland}/bin/hyprctl";
in rec {
  # We need to handle our own media keys

  louder = pkgs.writeShellScript "louder" ''
    ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 2%+ --limit ${maxVolume}
    unmute
  '';

  softer = pkgs.writeShellScript "softer"
    "${wpctl} set-volume @DEFAULT_AUDIO_SINK@ 2%- --limit ${maxVolume}";

  mute = pkgs.writeShellScript "mute"
    "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";

  unmute =
    pkgs.writeShellScript "unmute" "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ 0";

  play = pkgs.writeShellScript "play" "${playerctl} play-pause";

  pause = pkgs.writeShellScript "play" "${playerctl} pause";

  tao = pkgs.stdenv.mkDerivation {
    name = "tao";
    buildInputs = [ pkgs.python3 ];
    unpackPhase = "true";
    installPhase = ''
      cp ${./toggle_audio_output.py} $out
      substituteInPlace $out --replace 'wpctl' '${wpctl}'
    '';
  };

  # ...and brightness control

  brighter = pkgs.writeShellScript "brighter" "${brightnessctl} s +2%";
  darker = pkgs.writeShellScript "darker" "${brightnessctl} s 2%-";

  screenOn = pkgs.writeShellScript "screenOn" "${hyprctl} dispatch dpms on";
  screenOff = pkgs.writeShellScript "screenOff" "${hyprctl} dispatch dpms off";

  # ...and wm locking
  lock = pkgs.writeShellScript "lock" ''
    ${pkgs.swaylock-effects}/bin/swaylock -f -i ${commonOptions.backgroundImage} --indicator --clock --ring-color 8130d9 --key-hl-color 58e3f5 --fade-in 0.3 --grace 4  --effect-vignette 0.6:0.2\'
    # --effect-blur 10x5
  '';

  goodbye = "${pause} & ${lock}";
}
