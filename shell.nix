let
  pkgs = import <nixpkgs> {};

in pkgs.mkShell {
  buildInputs = [
    pkgs.gnupg
    pkgs.libiconv
    pkgs.stack
  ];
}
