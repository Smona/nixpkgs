{
  perSystem =
    { pkgs, ... }:
    {
      packages.claude-agent-acp = pkgs.buildNpmPackage (finalAttrs: {
        pname = "@zed-industries/claude-agent-acp";
        version = "0.108.1";

        src = pkgs.fetchFromGitHub {
          owner = "zed-industries";
          repo = "claude-agent-acp";
          tag = "v${finalAttrs.version}";
          hash = "sha256-yU4ywM4YQQfI1DK/DQ1cr9zERMaOOfFeF2JBtyz8xbg=";
        };

        npmDepsHash = "sha256-Cee29bOKbENsj7VnjIFQLT5M/yISbMOMYZkBkGDlVWw=";
      });
    };
}
