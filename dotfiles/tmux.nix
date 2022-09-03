{ config, lib, pkgs, ... }:

{
  home.shellAliases = {
    # Tmux aliases
    # tmux = "tmux -u";
    tma = "tmux attach -d -t";
    tml = "tmux ls";
    tmn = "tmux new -s";
  };

  programs.tmux = {
    enable = true;
    historyLimit = 500000;
    plugins = with pkgs.tmuxPlugins; [
      vim-tmux-navigator
      fpp
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-strategy-vim 'session'
          set -g @resurrect-processes '"~yarn start->yarn start"'
        '';
      }
      {
        plugin = continuum;
        extraConfig = "set -g @continuum-restore 'on'";
      }
      {
        plugin = onedark-theme;
        extraConfig = ''set -g @onedark_date_format "%D"'';
      }
      # {
      #   plugin = power-theme;
      #   extraConfig = "set -g @tmux_power_prefix_highlight_pos 'LR'";
      # }
      prefix-highlight
    ];
    extraConfig = ''
      # Mouse mode
      set-option -g mouse on
      # Fix color output
      set -g default-terminal 'tmux-256color'
      set -as terminal-overrides ',xterm*:Tc:sitm=\E[3m'

      # Easily reload config
      bind r source-file ~/.config/tmux/tmux.conf

      # don't rename windows automatically
      set-option -g allow-rename off

      # Vim style pane selection
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      # vim-tmux-resizer config
      is_vim='echo "#{pane_current_command}" | grep -iqE "(^|\/)g?(view|n?vim?x?)(diff)?$"'

      bind -n M-h if-shell "$is_vim" "send-keys ˙" "resize-pane -L 10"
      bind -n M-l if-shell "$is_vim" "send-keys ¬" "resize-pane -R 10"
      bind -n M-k if-shell "$is_vim" "send-keys ˚" "resize-pane -U 5"
      bind -n M-j if-shell "$is_vim" "send-keys ∆" "resize-pane -D 5"

      # split panes using | and -
      bind -n M-- split-window -h
      bind -n M-z split-window -v
      # unbind '"'
      # unbind %

      # Shift arrow to switch windows
      bind -n S-Left previous-window
      bind -n S-Right next-window

      # Switch between zoomed panes
      bind -r b select-pane -t .+1 \;  resize-pane -Z
      bind -r B select-pane -t .-1 \;  resize-pane -Z
    '';
  };

}
