{ pkgs, ... }:

{
  xdg.configFile."/home/mikolaj/.xmobarrc".source = ./config;
}

