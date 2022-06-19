{ lib, xdg-desktop-portal-wlr }:

xdg-desktop-portal-wlr.overrideAttrs
(oldAttrs: { patches = [ ./patches/xdg-desktop-portal-wlr-hyprland.patch ]; })
