{ pkgs, ... }:
let
  plugin = url: builtins.fetchGit {
    url = "https://github.com/${url}.git";
    ref = "HEAD";
  };
in {
programs.neovim = {
    enable = true;
    viAlias = true;
    extraConfig =
    ''
      lua << EOF
      ${builtins.readFile ./init.lua}
      EOF
    '';
    plugins = with pkgs.vimPlugins; [
      vim-which-key
      nvim-tree-lua
      YouCompleteMe
      vim-fugitive
      better-escape-nvim
      lualine-nvim
      nvim-web-devicons
      vim-commentary
      hop-nvim
      telescope-nvim
      bufferline-nvim
      tagbar
      gruvbox-nvim
      nvim-autopairs
      indent-blankline-nvim
      nvim-ts-rainbow
      vim-gitgutter
      nvim-autopairs
      (nvim-treesitter.withPlugins (
        plugins: with plugins; [
          tree-sitter-nix
        ]
      ))
      nvim-lspconfig
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      nvim-cmp
      cmp-vsnip
      vim-vsnip
    ];
  };
}
