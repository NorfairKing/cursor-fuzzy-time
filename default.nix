let
  pkgsv = import (import ./nix/nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/validity-version.nix)
    + "/nix/overlay.nix")
  );
  cursor-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/cursor-version.nix)
    + "/nix/overlay.nix")
  );
  fuzzy-time-overlay = import (
    (pkgs.fetchFromGitHub (import ./nix/fuzzy-time-version.nix)
    + "/nix/overlay.nix")
  );
in pkgsv {
  overlays = [
    validity-overlay
    cursor-overlay
    fuzzy-time-overlay
    ( import ./nix/gitignore-src.nix )
    ( import ./nix/overlay.nix )
  ];
  config.allowUnfree = true;
}
