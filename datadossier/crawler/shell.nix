let
  pkgs = import <nixpkgs> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    nodejs
    nodePackages.node2nix
  ];
}
