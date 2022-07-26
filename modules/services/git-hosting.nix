path:

{ config, lib, pkgs, ... }:

let
  inherit (lib) setAttrByPath mkEnableOption getAttrFromPath mkIf;
  inherit (pkgs) cgit;
  filters = "${cgit}/lib/cgit/filters";
in {
  options = setAttrByPath path {
    enable = mkEnableOption "Gitolite + cgit self-hosted Git server";
  };

  config = mkIf (getAttrFromPath path config).enable {
    services.gitolite = {
      enable = true;
      user = "git";
      group = "git";
      adminPubkey =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEBf93IkfW57z/aCDd+elE5nQ7LamVVDVAFr3n4oWPzj <ssh://git@git.adamorsomething.xyz|ed25519>";
      # Perl is a readonly language
      extraGitoliteRc = ''
        $RC{UMASK} = 0027;
        $RC{GIT_CONFIG_KEYS} = '.*';
        push( @{$RC{ENABLE}}, 'cgit' );
      '';
    };

    environment.etc.cgitrc.text = ''
      root-title=Ad's Repos
      root-desc=Random projects of mine

      css=/cgit/cgit.css
      logo=/cgit/cgit.png
      favicon=/cgit/favicon.ico

      enable-blame=1
      enable-commit-graph=1
      enable-git-config=1
      enable-index-links=1
      enable-log-filecount=1
      enable-log-linecount=1

      commit-filter=${filters}/commit-links.sh
      source-filter=${filters}/syntax-highlighting.py

      project-list=/var/lib/gitolite/projects.list
      scan-path=/var/lib/gitolite/repositories
    '';

    my.services.caddy.config = ''
      git.adamorsomething.xyz {
        handle_path /cgit* {
          root * ${cgit}/cgit
          file_server
        }
        handle {
          cgi * ${cgit}/cgit/cgit.cgi
        }
      }
    '';
  };
}
