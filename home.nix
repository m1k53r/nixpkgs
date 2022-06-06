{ config, pkgs ? import <nixpkgs> {}, ... }:

let
  haskell-env = with pkgs.haskell.packages.ghc8107; [
    hoogle
  ];
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "mikolaj";
  home.homeDirectory = "/home/mikolaj";

  fonts.fontconfig.enable = true;

  nixpkgs.overlays = [(self: super: { discord = super.discord.overrideAttrs (_: { src = builtins.fetchTarball https://discord.com/api/download?platform=linux&format=tar.gz; });})];

  home.packages = with pkgs; [
    # LSP
    rnix-lsp
    python-language-server
    pyright

    # Haskell stuff
    cabal-install
    cabal2nix
    stack
    ghc
    ghcid
    ormolu

    # Nix stuff
    nix-prefetch-git

    # Utils
    zlib
    zlib.dev

    # Fonts
    jetbrains-mono
    iosevka
    nerdfonts
    nerd-font-patcher
    
    # Other stuff
    chromium
    gnome3.gucharmap
    dconf
    python3
    ctags
    ripgrep
    android-studio
    jdk
    android-tools
    file
    nodejs-16_x
    fira-code-symbols
    xclip
    hlint
    bear
    discord
    mesa
    libpng
    xscreensaver
    scrot
    jq
    rofi
    papirus-icon-theme
    nmap
    tdesktop
    spotifywm
    flutter
    hover
    xmobar
    slock
    picom
    alacritty
    spotify
  ] ++ haskell-env;

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
    (import ./rofi)
    (import ./bash)
    (import ./xmonad)
    (import ./picom)
  ];
}
