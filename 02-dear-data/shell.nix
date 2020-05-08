with import <nixpkgs> {};

pkgs.mkShell {
  buildInputs = with pkgs; [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [ 
      svg-builder
    ]))
  ];
}
