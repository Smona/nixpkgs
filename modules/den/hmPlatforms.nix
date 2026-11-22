# Den configuration that adds hmLinux, hmDarwin, hmAarch64, and hm64bit classes
# for configuring packages installed via home manager that only work on some platforms.
#
{
  lib,
  den,
  ...
}:

let
  platforms = [
    "Linux"
    "Darwin"
    "Aarch64"
    "64bit"
  ];
  routes =
    system:
    let
      p = lib.systems.elaborate system;
    in
    map (
      name:
      den.lib.policy.route {
        fromClass = "hm${name}";
        intoClass = "homeManager";
        path = [ ];
      }
    ) (lib.filter (name: p."is${name}") platforms);
in
{
  den.classes = lib.genAttrs (map (n: "hm${n}") platforms) (n: {
    description = "homeManager config applied only on ${n} platforms";
  });

  den.policies.hm-platforms =
    {
      host ? null,
      home ? null,
      ...
    }:
    let
      system =
        if home != null then
          home.system
        else if host != null then
          host.system or null
        else
          null;
    in
    lib.optionals (system != null) (routes system);

  den.default.includes = [ den.policies.hm-platforms ];
}
