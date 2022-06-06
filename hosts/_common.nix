# TODO This is no longer dependent on anything not provided
# by a module, should separate into proper modules.

{ lib, inputs }: {
  options.my = let
    inherit (lib) mkOption;
    srcDir = ../.;
  in {
    srcDir = lib.mkDefault { default = srcDir; };
    configDir = mkOption { default = "${srcDir}/config"; };
    binDir = mkOption { default = "${srcDir}/bin"; };
  };
  config = {
    # I'm pretty sure I want the same time zone for every system I have,
    # and I can't think of anywhere better to put this, so I'm just
    # gonna keep it here until it starts causing me problems.
    time.timeZone = "America/New_York";

    system.configurationRevision = inputs.self.rev;
  };
}
