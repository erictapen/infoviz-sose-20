{
  description = "A very basic flake";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {

      packages.x86_64-linux = rec {
        vbb-crawler =
          let
            nodePackage = (
              import ./datadossier/crawler {
                inherit pkgs system;
                inherit (pkgs) nodejs;
              }
            ).package;
          in
          pkgs.writeShellScriptBin "vbb-crawler.sh" ''
            dir=$(date --iso-8601=date)
            mkdir -p $dir
            ${pkgs.nodejs}/bin/node ${nodePackage}/lib/node_modules/vbb-crawler/index.js \
              | ${pkgs.gzip}/bin/gzip -c \
              > "$dir/$(date --iso-8601=seconds).json.gz"
          '';
        diagram = pkgs.stdenv.mkDerivation {

          name = "diagram";
          src = ./datadossier/diagram;

          buildInputs = with pkgs; [
            (
              haskellPackages.ghcWithPackages (
                p: with p; [
                  aeson
                  zlib
                  utf8-string
                  geojson
                  MissingH
                  hcoord
                  svg-builder
                  streaming-osm
                  parallel
                  base64
                ]
              )
            )
            zlib
            inkscape
            imagemagick
          ];

          buildPhase = ''
            patchShebangs .
            ghc -O2 -o Main src/Main.hs
            ./Main
          '';

          installPhase = ''
            mkdir -p $out
            mv *.svg cache/*.svg cache/*.jpeg $out/
          '';
        };
        datadossier-website =
          let
            md = pkgs.copyPathToStore ./datadossier/website/index.markdown;
            css = pkgs.copyPathToStore ./datadossier/website/style.css;
          in
          pkgs.runCommand "datadossier-website"
            {
              buildInputs = [ pkgs.pandoc ];
              src = ./datadossier/website;
            } ''
            mkdir -p $out/images
            cd $src
            cp style.css $out/
            cp images/* $out/images/
            ln -s ${diagram}/2020-07-06_96.svg \
              ${diagram}/all_days_96.svg \
              ${diagram}/all_days_blended_96.svg \
              $out/images/
            pandoc -o $out/index.html --standalone --css style.css --webtex index.markdown
          '';
      };

      devShell.x86_64-linux =
        let
          pkgs = import nixpkgs { system = "x86_64-linux"; };
        in
        pkgs.mkShell {
          buildInputs = with pkgs; [

            # 04-create-a-visualization
            # jupyter
            python3Packages.pandas
            python3Packages.numpy
            python3Packages.altair
            python3Packages.SPARQLWrapper
            python3Packages.matplotlib

            # datadossier/crawler
            nodejs
            nodePackages.node2nix

            # datadossier/diagram
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
                  streaming-osm
                  parallel
                  base64
                ]
              )
            )
            stack
            ormolu # haskell code formatting
            zlib
            inkscape
            imagemagick

            # datadossier/reference-tracks
            cargo
            rustc
          ];
        };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.datadossier-website;

      nixosModules.vbb-crawler = import datadossier/crawler/module.nix;
    };
}
