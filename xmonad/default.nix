{ pkgs, ... }:

{
  xdg.configFile."/home/mikolaj/.xmonad/xmonad.hs".source = ./xmonad.hs;
}
