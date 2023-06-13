# Wraps every binary from a nixpkg with nixGL, as long as a nixGL
# prefix is set. This provides access to drivers so hardware acceleration
# works on non-NixOS systems.

# Call once on import to load global context
{ pkgs, config }:

# Wrap a single package
pkg:

if config.nixGLPrefix == "" then
  pkg
else
# Wrap the package's binaries with nixGL, while preserving the rest of
# the outputs and derivation attributes.
  (pkg.overrideAttrs (old: {
    name = "nixGL-${pkg.name}";
    # TODO: see if we can re-clear these inputs, since they cause extra build dependencies
    # to be pulled. I think we will need to nix all the other phases (installPhase etc),
    # because currently switching fails when i try to clear them here.
    # buildInputs = [ ];
    # nativeBuildInputs =
    #   (if old ? nativeBuildInputs then old.nativeBuildInputs else [ ])
    #   ++ [ pkgs.makeWrapper ];
    buildCommand = ''
      set -eo pipefail

      ${
      # Heavily inspired by https://stackoverflow.com/a/68523368/6259505
      # Copy original files, for each split-output (`out`, `dev` etc.).
      # E.g. `${package.dev}` to `$dev`, and so on. If none, just "out".
      # Symlink all files from the original package to here (`cp -rs`),
      # to save disk space.
      # We could alternatiively also copy (`cp -a --no-preserve=mode`).
      pkgs.lib.concatStringsSep "\n" (map (outputName: ''
        echo "Copying output ${outputName}"
        set -x
        cp -rs --no-preserve=mode "${pkg.${outputName}}" "''$${outputName}"
        set +x
      '') (old.outputs or [ "out" ]))}

      rm -rf $out/bin/*
      shopt -s nullglob # Prevent loop from running if no files
      for file in ${pkg.out}/bin/*; do
        echo "#!${pkgs.bash}/bin/bash" > "$out/bin/$(basename $file)"
        echo "exec -a \"\$0\" ${config.nixGLPrefix} $file \"\$@\"" >> "$out/bin/$(basename $file)"
        chmod +x "$out/bin/$(basename $file)"
      done
      shopt -u nullglob # Revert nullglob back to its normal default state
    '';
  }))
