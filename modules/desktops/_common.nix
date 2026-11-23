{
  # QWERTY has to come first for some steam games, so the empty
  # (default us) variant precedes dvorak.
  kbLayout = "us,us";
  kbVariant = ",dvorak";
  xkbOptions = [
    "terminate:ctrl_alt_bksp"
    "lv3:ralt_switch"
    "caps:swapescape"
    # Toggle layouts with Super+Space
    "grp:win_space_toggle"
  ];
}
