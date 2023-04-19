{ pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    shellAliases = {
      ecli = "emacsclient -c";
      c = "emacsclient -c ~/.config/nixpkgs/home.nix";
      pkgs = "nix repl '<nixpkgs>'";
      ls = "ls --color=auto";
      dir = "dir --color=auto";
      vdir = "vdir --color=auto";
      grep = "grep --color=auto";
      fgrep = "fgrep --color=auto";
      egrep = "egrep --color=auto";
      ll = "ls -alF";
      la = "ls -a";
      l = "ls -CF";
    };
    zplug = {
      enable = true;
      plugins = [
        { name = "marlonrichert/zsh-autocomplete"; }
        { name = "zsh-users/zsh-autosuggestions"; }
        { name = "zsh-users/zsh-syntax-highlighting"; }
        { name = "mafredri/zsh-async"; }
        { name = "sindresorhus/pure"; tags = [ use:pure.zsh as:theme ]; }
      ];
    };
  };
}
