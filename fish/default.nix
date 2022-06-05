{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    shellInit = ''
    function vterm_printf;
        if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end 
            # tell tmux to pass the escape sequences through
            printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
        else if string match -q -- "screen*" "$TERM"
            # GNU screen (screen, screen-256color, screen-256color-bce)
            printf "\eP\e]%s\007\e\\" "$argv"
        else
            printf "\e]%s\e\\" "$argv"
        end
    end
    set -gx BG        "#3F3F3F"
    set -gx BGALT     "#494949"
    set -gx FG        "#DCDCCC"
    set -gx RED    	  "#CC9393"
    set -gx ORANGE 	  "#DFAF8F"
    set -gx YELLOW 	  "#F0DFAF"
    set -gx GREEN  	  "#7F9F7F"
    set -gx CYAN   	  "#93E0E3"
    set -gx BLUE   	  "#8CD0D3"
    set -gx MAGENTA	  "#DC8CC3"
    '';
    shellAliases = {
        pkgs = "nix repl \"<nixpkgs>\"";
        ecli = "emacsclient -c";
    };
  };

  xdg.configFile."fish/functions/fish_prompt.fish".source = ./prompt;
}
