{ config, pkgs ? import <nixpkgs> {}, ... }:

let
  haskell-env = with pkgs.haskell.packages.ghc8107; [
    hoogle
  ];
  python-env = with pkgs.python3Packages; [ pwntools ];
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
    haskell-language-server
    python-language-server
    pyright
    elixir_ls

    # Languages
    elixir
    python3
    nodejs-16_x
    flutter
    ocaml
    rustc
    
    # Rust stuff
    cargo
    rust-analyzer

    # Ocaml stuff
    opam
    dune_3

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
    font-awesome
    material-design-icons
    weather-icons
    emacs-all-the-icons-fonts
    
    # Other stuff
    chromium
    gnome3.gucharmap
    dconf
    ctags
    ripgrep
    android-studio
    jdk
    android-tools
    file
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
    hover
    xmobar
    slock
    picom
    alacritty
    spotify
    zsh
    steam-run
    docker-compose
    imagemagick
    inotify-tools
    unzip
    exercism
    ngrok
    obsidian
    gef
    gdb
    docker
    postgresql
  ] ++ haskell-env ++ python-env;

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

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.bash.enable = true;
  # OR
  programs.zsh.enable = true;
  # Or any other shell you're using

  imports = [
    (import ./tmux) 
    (import ./xmobar) 
    (import ./alacritty) 
    (import ./fish) 
    (import ./git)
    (import ./emacs)
    (import ./code)
    (import ./rofi)
    (import ./bash)
    (import ./xmonad)
    (import ./nvim)
    (import ./picom)
    (import ./zsh)
    (import ./wallpaper)
  ];
}
