{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
  };

  xdg.configFile."fish/functions/fish_prompt.fish".source = ./prompt;
}
