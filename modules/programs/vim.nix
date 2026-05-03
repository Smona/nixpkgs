# My personal vim config
{ ... }:
{
  flake.homeModules.vim =
    { pkgs, ... }:
    {
      programs.vim = {
        enable = true;
        settings = {
          # Enable mouse navigation
          mouse = "a";
          # tabs are spaces
          expandtab = true;
          # number of spaces when reindinting
          shiftwidth = 2;
          # number of visual spaces per TAB
          tabstop = 2;
          ignorecase = true;
          smartcase = true;
          # Allow hiding unsaved buffers
          hidden = true;
        };
        plugins = with pkgs.vimPlugins; [
          vim-commentary
          vim-repeat
          vim-surround
        ];
        extraConfig = ''
          set nocompatible              " be iMproved
          :let mapleader = ","

          set splitbelow
          set splitright

          set encoding=utf-8
          set autoindent

          " Indent Settings
          set softtabstop=2       " number of spaces in tab when editing

          " Search Settings
          set incsearch           " search as characters are entered
          set hlsearch            " highlight matches
          " turn off search highlight
          nnoremap <leader><space> :nohlsearch<CR>

          inoremap <C-c> <Esc>

          set t_Co=256            " Use 256 colours always

          " Theme choice, with optimizations for gruvbox
          set termguicolors
          set background=dark

          " Use hybrid line numbers while editing file, and absolute otherwise
          " NOTE: disabled to improve performance
          set number " relativenumber
          " augroup numbertoggle
          "   autocmd!
          "   autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
          "   autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
          " augroup END
        '';
      };
    };
}
