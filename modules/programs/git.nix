# My personal git config
{ ... }:
{
  flake.homeModules.git =
    {
      config,
      lib,
      ...
    }:
    {
      programs.git = {
        enable = true;
        signing = {
          signByDefault = true;
          key = "290FCF081AEDB3EC";
        };
        includes = [
          # Use separate file for username / github token / etc
          { path = "~/.gitconfig.local"; }
        ];
        settings = {
          user = {
            name = "Mel Bourgeois";
            email = "mason.bourgeois@gmail.com";
          };
          core.autocrlf = "input";
          init.defaultBranch = "main";
          pull.rebase = true;
          fetch.prune = true;
          rebase.autoStash = true;
          merge.autoStash = true;
          # Correct typos
          help.autocorrect = 1;
          diff = {
            # Use different colors for moved code vs additions/deletions.
            colorMoved = "zebra";
            # Diff bun lockfiles legibly, according to docs. Fun setup step! 🙄
            lockb = {
              textconv = "bun";
              binary = true;
            };
          };
          # Forges
          github.user = "Smona";
          gitlab.user = "Smona";
        };
        attributes = [ "*.lockb binary diff=lockb" ];
        ignores = [
          ".gitconfig.local" # Local config file
          "private" # General-purpose private directories
          # Folder view configuration files
          ".DS_Store"
          "Desktop.ini"
          # Thumbnail cache files
          "._*"
          "*~"
          "Thumbs.db"
          # Files that might appear on external disks
          ".Spotlight-V100"
          ".Trashes"
          # Compiled Python files
          "*.pyc"
          # npm & bower
          "bower_components"
          "node_modules"
          "npm-debug.log"
          "yarn-error.log"
          # IDEs stuff
          ".idea"
          ".log"
          ".agent-shell"
        ];
      };
      # Fancier git diffs
      programs.delta = {
        enable = true;
        enableGitIntegration = true;
        # NB: adding line-numbers feature breaks magit-delta
        # https://github.com/dandavison/magit-delta/issues/13
        options = {
          features = lib.mkForce "side-by-side decorations catppuccin-${config.catppuccin.flavor}";
          hyperlinks = true;
        };
      };
    };
}
