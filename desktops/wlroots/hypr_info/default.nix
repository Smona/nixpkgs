{ pkgs ? import <nixpkgs> { }, ... }:

let python = pkgs.python310.withPackages (p: [ p.pygobject3 ]);
in python.env
