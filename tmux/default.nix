{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    clock24 = true;
    extraConfig = ''
    setw -g mode-keys vi
    '';
  };
}
