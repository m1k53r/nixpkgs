{ config, lib, pkgs, ... }:
{
  xdg.configFile."/home/mikolaj/.config/rofi/config.rasi".source = ./config;
}
