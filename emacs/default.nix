
{ config, lib, pkgs, ... }:

let
  cfg = config.programs.emacs;

  emacsPackages = let epkgs = pkgs.emacsPackagesFor cfg.package;
  in epkgs.overrideScope' cfg.overrides;

in {
    services.emacs.enable = true;
    programs.emacs = {
    enable = true; 
    #extraConfig = ''
    #  (setq standard-indent 2)
    #  (require 'evil)
    #  (evil-mode 1)

    #  (display-line-numbers 1)
    #  (setq display-line-numbers 'relative)
    #'';

    extraPackages = epkgs: [
      # Utils
      epkgs.evil
      epkgs."darkburn-theme"
      epkgs.helm
      epkgs."use-package"
      epkgs."which-key"
      epkgs.company
      epkgs."all-the-icons"
      epkgs."all-the-icons-dired"
      epkgs."ace-window"
      epkgs."ace-jump-mode"
      epkgs.sublimity

      # Org

      epkgs.olivetti
      epkgs."org-roam"
      epkgs."org-bullets"
      epkgs."org-appear"
      epkgs.deft
      epkgs."org-download"
      epkgs.typo
      epkgs."org-beautify-theme"
      epkgs."evil-org"

      # LSP

      epkgs."lsp-mode"
      epkgs."lsp-ui"
      epkgs."lsp-python-ms"
      epkgs."lsp-haskell"
      epkgs."lsp-dart"

      # Modes

      epkgs."nix-mode"
      epkgs."haskell-mode"
      epkgs."typescript-mode"
      epkgs."python-mode"
      epkgs."web-mode"
      epkgs."dart-mode"

      # Dev tools

      epkgs.flycheck
      epkgs.magit
      epkgs.projectile
      epkgs.tide
      epkgs."undo-fu"
      epkgs."rainbow-delimiters"
      epkgs.yasnippet
      epkgs."yasnippet-snippets"
      epkgs."indent-guide"
      epkgs.dimmer
      epkgs."company-box"
      epkgs."evil-nerd-commenter"
      epkgs."git-gutter"
      epkgs."fringe-helper"
      epkgs."git-gutter-fringe"
      epkgs."smart-mode-line"
      epkgs."shell-switcher"
      epkgs."centaur-tabs"
      epkgs.shackle
      epkgs.popper
      epkgs."lsp-treemacs"
    ];
  };
  xdg.configFile."/home/mikolaj/.emacs.d/init.el".source = ./init.el;
}


# { pkgs, ... }:
# {
#  xdg.configFile."/home/mikolaj/.emacs.d/init.el".source = ./init.el;
#}
