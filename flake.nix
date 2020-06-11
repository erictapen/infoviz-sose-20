{
  description = "A very basic flake";

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in
    {

      packages.x86_64-linux = {
        vbb-crawler = let
          nodePackage = (
            import ./datadossier/crawler {
              inherit pkgs system;
              inherit (pkgs) nodejs;
            }
          ).package;
        in
          pkgs.writeShellScriptBin "vbb-crawler.sh" ''
            ${pkgs.nodejs}/bin/node ${nodePackage}/lib/node_modules/vbb-crawler/index.js \
              | ${pkgs.gzip}/bin/gzip -c \
              > "$(date --iso-8601=seconds).json.gz"
      '';
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.vbb-crawler;

      nixosModules.vbb-crawler = import datadossier/crawler/module.nix;
    };
}
