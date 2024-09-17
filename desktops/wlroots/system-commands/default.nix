{ pkgs, ... }:

let
  maxVolume = "1.2";
  volumeStepPercent = "4";
  brightnessStepPercent = "4";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  wpctl = "${pkgs.wireplumber}/bin/wpctl";
  hyprctl = "${pkgs.hyprland}/bin/hyprctl";
in
rec {
  # We need to handle our own media keys

  louder = pkgs.writeShellScript "louder" ''
    ${wpctl} set-volume @DEFAULT_AUDIO_SINK@ ${volumeStepPercent}%+ --limit ${maxVolume}
    ${unmute}
  '';

  softer = pkgs.writeShellScript "softer" "${wpctl} set-volume @DEFAULT_AUDIO_SINK@ ${volumeStepPercent}%- --limit ${maxVolume}";

  mute = pkgs.writeShellScript "mute" "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ toggle";

  unmute = pkgs.writeShellScript "unmute" "${wpctl} set-mute @DEFAULT_AUDIO_SINK@ 0";

  play = pkgs.writeShellScript "play" "${playerctl} play-pause";

  pause = pkgs.writeShellScript "pause" "${playerctl} pause";

  prev = pkgs.writeShellScript "prev" "${playerctl} previous";
  next = pkgs.writeShellScript "next" "${playerctl} next";

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

  brighter = pkgs.writeShellScript "brighter" "${brightnessctl} s +${brightnessStepPercent}%";
  darker = pkgs.writeShellScript "darker" "${brightnessctl} s ${brightnessStepPercent}%-";

  # TODO: be smarter about this, use the right command per-wm
  screenOn = pkgs.writeShellScript "screenOn" "${hyprctl} dispatch dpms on";
  screenOff = pkgs.writeShellScript "screenOff" "${hyprctl} dispatch dpms off";

  # ...and wm locking
  # note that pause will error if nothing is playing, so we can't use &&
  lock = "${pause}; hyprlock";
}
