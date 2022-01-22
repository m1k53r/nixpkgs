{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mikolaj";
  home.homeDirectory = "/home/mikolaj";

  home.packages = with pkgs; [
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
    gcc
    ncurses
  ];

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
  ];
}
