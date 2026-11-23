{
  inputs,
  den,
  lib,
  ...
}:

{
  imports = [ inputs.den.flakeModule ];

  den.schema.user = {
    classes = lib.mkDefault [ "homeManager" ];
  };

  # Allow my own user to configure nixos options across all hosts.
  den.schema.host.includes = [
    (den.batteries.user-aspects {
      users = [
        "mel"
        "smona"
      ];
    })
  ];

  den.default = {
    includes = [
      den.batteries.define-user
      den.batteries.hostname
    ];

    # Setting consistent global stateVersion defaults, using the oldest
    # stateVersion in the repo.
    #
    # Before changing these values read their documentation
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html),
    # and perform any necessary migrations on all hosts.
    nixos.system.stateVersion = lib.mkDefault "22.05";
    homeManager.home.stateVersion = lib.mkDefault "22.05";
    darwin.system.stateVersion = lib.mkDefault 4;
  };
}
