{ config, pkgs ? import <nixpkgs> {}, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mikolaj";
  home.homeDirectory = "/home/mikolaj";

  nixpkgs.overlays = [(self: super: { discord = super.discord.overrideAttrs (_: { src = builtins.fetchTarball https://discord.com/api/download?platform=linux&format=tar.gz; });})];

  home.packages = with pkgs; [
    # LSP
    rnix-lsp
    haskell-language-server
    rls
    python-language-server

    # Other stuff
    htop
    chromium
    gnome3.gucharmap
    dconf
    ghc
    haskell-language-server
    python3
    pyright
    ctags
    ripgrep
    clang
    gnumake
    android-studio
    jdk
    android-tools
    file
    nodejs-16_x
    fira-code-symbols
    xclip
    hlint
    ccls
    bear
    clang-tools
    cmake
    discord
  ];

  nixpkgs.config.allowUnfree = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = [
    (import ./tmux) 
    (import ./xmobar) 
    (import ./alacritty) 
    (import ./fish) 
    (import ./nvim)
    (import ./git)
    (import ./emacs)
    (import ./code)
  ];
}
