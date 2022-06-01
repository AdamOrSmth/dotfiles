{ config, lib, pkgs, ... }:

let inherit (pkgs) alacritty brightnessctl dunst eww lightlocker polybar rofi;
in {
  config = lib.mkIf (!config.ad.isServer) {
    services = {
      xserver = {
        enable = true;
        windowManager.bspwm.enable = true;

        # CapsLock to Escape
        xkbOptions = if config.ad.isLaptop then "caps:escape" else "";
      };

      picom = {
        enable = true;
        backend = "glx"; # Newer backend
        vSync = true;
        # Blur is hot
        settings = {
          blur = {
            method = "dual_kawase";
            strength = 3;
          };
        };
        # Blur requires experimental backends
        experimentalBackends = true;
      };
    };
    systemd.sleep.extraConfig =
      "HibernateDelaySec=1h"; # Make `suspend-then-hiberate` hibernate after an hour

    environment.systemPackages =
      (lib.optionals config.ad.isLaptop [ brightnessctl ])
      ++ [ alacritty dunst eww lightlocker polybar rofi ];
  };
}
