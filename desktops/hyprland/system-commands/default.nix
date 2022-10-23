{ pkgs, ... }:

let
  maxVolume = "1.2";
  playerctl = "${pkgs.playerctl}/bin/playerctl";
  brightnessctl = "${pkgs.brightnessctl}/bin/brightnessctl";
  wpctl = "${pkgs.wireplumber}/bin/wpctl";
in {
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
}
