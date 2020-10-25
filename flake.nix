{
  description = "A very basic flake";

  inputs = {
    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-mozilla }:
    let
      forAllSystems = f: nixpkgs.lib.genAttrs
        [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system: f system);
      nixpkgsFor = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [ (import nixpkgs-mozilla) ];
        }
      );
    in
    {

      packages = forAllSystems (system:
        let
          pkgs = nixpkgsFor.${system};
        in
        rec {
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
                    parallel
                    base64
                    parallel-io
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
        });

      devShell = forAllSystems
        (system:
          let
            pkgs = nixpkgsFor.${system};
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
                    parallel
                    base64
                    parallel-io
                  ]
                )
              )
              stack
              ormolu # haskell code formatting
              zlib
              inkscape
              imagemagick

              qgis
            ] ++ (
              let
                rustUnstable = (pkgs.rustChannelOf {
                  date = "2020-10-10";
                  channel = "nightly";
                  sha256 = "sha256-PLdvfPsf813gJu5UbcQv9+6zig3KZOvJHw0ZF1xvWoU=";
                });
              in
              [
                rustUnstable.cargo
                rustUnstable.rust
              ]
            );
          });

      defaultPackage = forAllSystems (system: self.packages.${system}.datadossier-website);

      nixosModules.vbb-crawler = import datadossier/crawler/module.nix;
    };
}
