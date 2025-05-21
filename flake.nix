{
  description = "OCaml gRPC library";

  inputs = {
    nixpkgs = {
      url = "github:nix-ocaml/nix-overlays/bf4dbbb8793e72575f07489e317cc6309bca7f17";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-filter.url = "github:numtide/nix-filter";
    ocaml-overlay.url = "github:nix-ocaml/nix-overlays/a6364bea92bb35b01a3a70eed9a5cdb1063e128e";
    ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {
    flake-parts,
    nix-filter,
    ocaml-overlay,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];

      imports = [inputs.flake-parts.flakeModules.easyOverlay];

      perSystem = {
        config,
        self',
        inputs',
        system,
        ...
      }: let
        pkgs =
          ((import inputs.nixpkgs {
              inherit system;
              config.allowUnfree = true;
              overlays = [ocaml-overlay.outputs.overlays];
            })
            .extend (import ./overlay.nix))
          .extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_1;
          });
        camlPkgs = pkgs.ocaml-ng.ocamlPackages_5_1;
      in {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            self'.packages.grpc
            self'.packages.grpc-examples
          ];
          nativeBuildInputs = with pkgs; [
            nil
            nixfmt
            camlPkgs.ocaml-lsp
            camlPkgs.ocamlformat
            camlPkgs.ocaml-protoc
          ];
        };

        packages = {
          grpc-examples = camlPkgs.buildDunePackage {
            pname = "grpc-examples";
            version = "0.1.0";
            duneVersion = "3";
            nativeBuildInputs = with camlPkgs; [
              ppx_jane
              ppx_deriving
              ppx_deriving_yojson
            ];
            buildInputs = with camlPkgs; [
              conduit-lwt-unix
              core_unix
              ppx_deriving_yojson
              cohttp-lwt-unix
              tls-async
              self'.packages.grpc
            ];
            src = nix-filter.lib.filter {
              root = ./.;
              include = ["dune-project" "examples"];
            };
          };
          grpc = camlPkgs.buildDunePackage {
            pname = "grpc";
            version = "0.1.0";
            duneVersion = "3";
            nativeBuildInputs = with camlPkgs; [mdx];
            propagatedBuildInputs = with camlPkgs; [ppxlib];
            buildInputs = with camlPkgs; [uri haha hpackv ppx_deriving];
            src = nix-filter.lib.filter {
              root = ./.;
              include = ["dune-project" "lib/grpc"];
            };
          };
        };
        packages.default = self'.packages.grpc;
      };
    };
}
