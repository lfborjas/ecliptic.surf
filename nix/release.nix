let
  pkgs = import ./packages.nix {};
in
  { ecliptic-surf = pkgs.haskellPackages.ecliptic-surf; }
