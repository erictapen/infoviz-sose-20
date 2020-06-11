{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in  {

    packages.x86_64-linux = {
      vbb-crawler = (import ./datadossier/crawler {
        inherit pkgs system;
        inherit (pkgs) nodejs;
      }).package;
    };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.vbb-crawler;

    nixosModules.vbb-crawler = import datadossier/crawler/module.nix;
  };
}
