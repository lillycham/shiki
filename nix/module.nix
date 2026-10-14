{ self }:
{ config, lib, pkgs, ... }:
let
  cfg = config.services.shiki;
  configFile = pkgs.writeText "shiki.json" (builtins.toJSON {
    inherit (cfg) host port probes;
  });
in
{
  options.services.shiki = {
    enable = lib.mkEnableOption "shiki, small probes that serve their results as JSON";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      description = "The shiki package.";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "The address to listen on. Keep it on loopback, and reach it through a proxy such as `tailscale serve`.";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8092;
      description = "The port to listen on.";
    };

    probes = lib.mkOption {
      type = lib.types.listOf (lib.types.attrsOf lib.types.anything);
      default = [ ];
      example = lib.literalExpression ''
        [
          { name = "backup"; group = "notes"; kind = "launchd"; label = "notes-backup"; }
          { name = "site"; group = "web"; kind = "http"; url = "http://127.0.0.1:8080/"; every = 30; }
          { name = "backups"; group = "notes"; kind = "file-age"; path = "/home/me/backups"; "warn-after" = 26 * 3600; }
        ]
      '';
      description = ''
        The probes, as in the README: each has a name, a kind, and the fields
        of its kind. Optional: group, every and timeout (seconds).
      '';
    };

    logFile = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/Library/Logs/shiki.log";
      description = "Where the launchd agent writes its output (macOS).";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    launchd.agents.shiki = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
      enable = true;
      config = {
        ProgramArguments = [ "${cfg.package}/bin/shiki" "serve" "${configFile}" ];
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Background";
        StandardOutPath = cfg.logFile;
        StandardErrorPath = cfg.logFile;
      };
    };

    systemd.user.services.shiki = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      Unit.Description = "shiki probes";
      Service = {
        ExecStart = "${cfg.package}/bin/shiki serve ${configFile}";
        Restart = "on-failure";
      };
      Install.WantedBy = [ "default.target" ];
    };
  };
}
