{ pkgs, ...}:
{
  programs.git = {
    enable = true;
    userName = "m1k53r";
    userEmail = "mikobw369@protonmail.com";
    aliases = {
      st = "status";
    };
  };
}
