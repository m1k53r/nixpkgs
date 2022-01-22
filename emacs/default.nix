{ config, lib, pkgs, ... }:

let
  cfg = config.programs.emacs;

  emacsPackages = let epkgs = pkgs.emacsPackagesFor cfg.package;
  in epkgs.overrideScope' cfg.overrides;

in {
  programs.emacs = {
    enable = true; 
    #extraConfig = ''
    #  (setq standard-indent 2)
    #  (require 'evil)
    #  (evil-mode 1)
    #'';

    extraPackages = epkgs: [
      epkgs.evil
      epkgs."gruvbox-theme"
    ];
  };
  xdg.configFile."/home/mikolaj/.emacs.d/init.el".source = ./init.el;
}


# { pkgs, ... }:
# {
#  xdg.configFile."/home/mikolaj/.emacs.d/init.el".source = ./init.el;
#}
