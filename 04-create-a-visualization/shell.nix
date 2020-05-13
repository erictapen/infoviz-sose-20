let
  pkgs = import <nixpkgs> {};
in
# start with `jupyter-notebook --no-browser`
pkgs.mkShell {
  buildInputs = (with pkgs; [ jupyter ])
  ++ (
    with pkgs.python3Packages; [
      pandas
      numpy
      altair
      SPARQLWrapper
      matplotlib
    ]
  );
}
