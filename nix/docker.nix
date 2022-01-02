{ pkgs ? import ./packages.nix { system = "x86_64-linux"; } }:

let
  bin = (pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontCheck pkgs.haskellPackages.ecliptic-surf));
  config = ../config;
  static = ../static;
in

# This is the nix api to build images
pkgs.dockerTools.buildImage {
  # our image name
  name = "ecliptic-surf";
  # our image tag
  tag = "latest";
  
  # this is a list of the things we want to include
  # in the image. it's incredibly bare by default.
  contents = [
    bin # our app
    pkgs.tzdata
  ];
  fromImage = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:e1871801d30885a610511c867de0d6baca7ed4e6a2573d506bbec7fd3b03873f";
    sha256 = "0ymhp3hrhpf7425n3awz6b67510x9xcgpldi4xm610aqfk1rygy9";
  };
  # from: https://blog.codeaddict.org/2020/06/01/build-a-nix-docker-image-in-gitlab-ci/
  # copy the config folder in its entirety, also link the nix-provided tzdata
  # to the standard location as expected (perhaps erroneously) by timezone-detect:
  # https://github.com/lfborjas/timezone-detect/blob/6c3d7954431b63c07c1b0018a364897e42080e54/src/Data/Time/LocalTime/TimeZone/Detect.hs#L123
  #
  # NOTE(luis) not sure why this particular application isn't picking up
  # the environment variables _or_ the explicit setting of the precalc
  # location... but I'm running out of time so for now just copying to `/home`
  extraCommands = ''
    cp -rf ${config} config
    cp -rf ${static} static
    chmod -R 777 config
    cp -r /config/ephe/ home
    mkdir -p usr/share
    ln -sf ${pkgs.tzdata}/share/zoneinfo usr/share/zoneinfo
  '';

  # This exposes the Dockerfile commands you might be familiar with
  config = {
    Cmd = [ "${bin}/bin/ecliptic-surf-exe" ];
    Env = [ 
      "DEPLOY_ENV=Production"
      "TIMEZONE_DATABASE_FILE=/config/timezone21.bin"
      "SE_EPHE_PATH=/config/ephe"
      "EP4_PATH=/config/ephe"
    ];
  };
}
