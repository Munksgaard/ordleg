let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in
with pkgs;
mkShell {
  name = "ordleg";
  buildInputs = [
    nix
    nodejs-14_x
    gmp
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
  ];
}
