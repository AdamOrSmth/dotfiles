# Common configuration module for all my hosts.
# Still debating on whether this should stay here in `hosts/_common.nix`,
# or in a `host.nix` file in the root, but I'm just gonna keep it here
# for the time being.

{ host, lib }: {
  options.my = let
    inherit (lib) mkOption;
    srcDir = ../.;
  in {
    # I need a way to refer to the top-level paths of my Flakes directory
    # from individual modules, but I didn't feel like adding an entire module
    # for the sole purpose of exporting paths, so let's do it here instead.
    # I'm gonna forget this is here and then be like "where the hell are
    # these variables getting initialized".
    srcDir = mkOption { default = srcDir; };
    configDir = mkOption { default = "${srcDir}/config"; };
    binDir = mkOption { default = "${srcDir}/bin"; };
  };
  config = {
    # Hostname should always be the same, so it's only bootstrapped once during
    # initial setup.
    networking.hostName = host;
    # I'm pretty sure I want the same time zone for every system I have,
    # and I can't think of anywhere better to put this, so I'm just
    # gonna keep it here until it starts causing me problems.
    time.timeZone = "America/New_York";
  };
}
