with import <nixpkgs> {};

pkgs.mkShell {
  buildInputs = with pkgs; [
    (
      pkgs.haskellPackages.ghcWithPackages (
        p: with p; [
          aeson
          zlib
          utf8-string
          geojson
          MissingH
          hcoord
          svg-builder
        ]
      )
    )
    stack
    ormolu # haskell code formatting
    zlib
  ];
}
