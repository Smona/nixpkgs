{
  perSystem =
    {
      pkgs,
      self',
      runTest,
      ...
    }:
    {
      # likec4 CLI packaged for NixOS.
      packages.likec4 =
        let
          # It is recommended to pin pnpm to a major version, due to regular breaking changes in the store format
          pnpm = pkgs.pnpm_12;
        in
        pkgs.stdenv.mkDerivation (finalAttrs: {
          pname = "likec4";
          version = "1.61.0";

          src = pkgs.fetchFromGitHub {
            owner = "likec4";
            repo = "likec4";
            tag = "v${finalAttrs.version}";
            hash = "sha256-pgtpakgfoXaFwlB0bKnWw7GZd1FTAafKU4CQzh5RVRI=";
          };

          nativeBuildInputs = [
            pkgs.nodejs # in case scripts are run outside of a pnpm call
            pkgs.pnpmConfigHook
            pkgs.makeWrapper
            pnpm # At least required by pnpmConfigHook, if not other (custom) phases
          ];

          pnpmWorkspaces = [
            "likec4"
            "@likec4/style-preset"
            "@likec4/log"
            "@likec4/devops"
            "@likec4/layouts"
            "@likec4/icons"
            "@likec4/core"
            "@likec4/mcp"
            "@likec4/language-server"
            "@likec4/language-services"
            "@likec4/styles"
            "@likec4/config"
            "@likec4/leanix-bridge"
            "@likec4/diagram"
            "@likec4/generators"
            "@likec4/react"
            "@likec4/vite-plugin"
            "@likec4/spa"
          ];
          pnpmDeps = pkgs.fetchPnpmDeps {
            inherit (finalAttrs)
              pname
              version
              src
              pnpmWorkspaces
              ;
            inherit pnpm;
            fetcherVersion = 5;
            hash = "sha256-KM0JMfkQFn4X/dhMN3MjwaKgywvf/8HB1729RAPYh0E=";
          };

          postPatch = ''
            # Give vite a writeable cache dir
            substituteInPlace packages/likec4/src/vite/vite-dev.ts \
                --replace-fail 'const server = await createServer({' \
                'const server = await createServer({ cacheDir: (process.env.XDG_CACHE_HOME || (process.env.HOME + "/.cache")) + "/likec4/vite",'
          '';

          # Build only the CLI package
          buildPhase = ''
            runHook preBuild
            pnpm exec turbo run build --filter=likec4...
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall

            mkdir -p $out/share $out/bin

            cp package.json $out/share/
            cp -r packages $out/share/
            cp -r apps $out/share/
            cp -r styled-system $out/share/
            cp -r devops $out/share/
            cp -r node_modules $out/share/

            makeWrapper ${pkgs.nodejs}/bin/node $out/bin/likec4 \
              --add-flags "$out/share/packages/likec4/bin/likec4.mjs"

            runHook postInstall
          '';
        });
    };
}
