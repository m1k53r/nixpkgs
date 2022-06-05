{ config, lib, pkgs, ... }:
{
  programs.bash = {
    enable = true;
    sessionVariables = {
        BG      = "#3F3F3F"; 
        BGALT   = "#494949";
        FG      = "#DCDCCC";
        RED     = "#CC9393";
        ORANGE  = "#DFAF8F";
        YELLOW  = "#F0DFAF";
        GREEN   = "#7F9F7F";
        CYAN    = "#93E0E3";
        BLUE    = "#8CD0D3";
        MAGENTA = "#DC8CC3";
    };
  };
}
