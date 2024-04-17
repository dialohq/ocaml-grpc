{
  description = "Description for the project";

  inputs = {
    nixpkgs = {
      url =
        "github:nix-ocaml/nix-overlays/bf4dbbb8793e72575f07489e317cc6309bca7f17";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nix-filter.url = "github:numtide/nix-filter";
    ocaml-overlay.url =
      "github:nix-ocaml/nix-overlays/a6364bea92bb35b01a3a70eed9a5cdb1063e128e";
    ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ flake-parts, nix-filter, ocaml-overlay, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      imports = [ inputs.flake-parts.flakeModules.easyOverlay ];

      perSystem = { config, self', inputs', system, ... }:
        let
          pkgs = (((import inputs.nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [ ocaml-overlay.outputs.overlays ];
          })).extend (import ./overlay.nix)).extend (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_1;
          });
          camlPkgs = pkgs.ocaml-ng.ocamlPackages_5_1;
          bechamel-notty = camlPkgs.buildDunePackage {
            pname = "bechamel-notty";
            version = "0.5.0";
            duneVersion = "3";
            propagatedBuildInputs =
              [ camlPkgs.notty camlPkgs.fmt camlPkgs.bechamel ];
            src = pkgs.fetchFromGitHub {
              owner = "mirage";
              repo = "bechamel";
              rev = "v0.5.0";
              sha256 = "sha256-aTz80gjVi+ITqi8TXH1NjWPECuTcLFvTEDC7BoRo+6M=";
              fetchSubmodules = true;
            };
          };
          dialo-ocaml-protoc-plugin = camlPkgs.buildDunePackage {
            pname = "ocaml-protoc-plugin";
            version = "0.1.0";
            duneVersion = "3";

            INCLUDE_GOOGLE_PROTOBUF = "${pkgs.protobuf}/include";

            nativeBuildInputs = [ pkgs.protobuf ];
            propagatedBuildInputs = [ pkgs.protobuf pkgs.pkg-config ];
            buildInputs = with camlPkgs; [ lwt stringext ];
            src = pkgs.fetchFromGitHub {
              owner = "dialohq";
              repo = "ocaml-protoc-plugin";
              rev = "b814b305520563fff58388682cb360660cc29c47";
              sha256 = "sha256-NgFvc+HTJXc17GwyfA0VqlWXx9R35FJ6CSEQrQ52Jds=";
              fetchSubmodules = true;
            };
          };

        in {
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              self'.packages.grpc
              self'.packages.grpc-lwt
              self'.packages.grpc-async
              self'.packages.grpc-eio
              self'.packages.grpc-examples
              self'.packages.grpc-bench
            ];
            nativeBuildInputs = with pkgs; [
              nil
              nixfmt
              camlPkgs.ocaml-lsp
              camlPkgs.ocamlformat
            ];
          };

          packages = {
            grpc-bench = camlPkgs.buildDunePackage {
              pname = "grpc-bench";
              version = "0.1.0";
              duneVersion = "3";
              buildInputs = with camlPkgs; [
                self'.packages.grpc
                self'.packages.grpc-lwt
                self'.packages.grpc-async
                self'.packages.grpc-eio
                bechamel-notty
                bigstringaf
              ];
              src = nix-filter.lib.filter {
                root = ./.;
                include = [ "dune-project" "examples" ];
              };
            };
            grpc-examples = camlPkgs.buildDunePackage {
              pname = "grpc-examples";
              version = "0.1.0";
              duneVersion = "3";
              nativeBuildInputs = with camlPkgs; [
                dialo-ocaml-protoc-plugin
                ppx_jane
                ppx_deriving
                ppx_deriving_yojson
              ];
              buildInputs = with camlPkgs; [
                h2-lwt-unix
                conduit-lwt-unix
                core_unix
                ppx_deriving_yojson
                cohttp-lwt-unix
                camlPkgs.h2-eio
                camlPkgs.h2-async
                tls-async
                self'.packages.grpc
                self'.packages.grpc-lwt
                self'.packages.grpc-async
                self'.packages.grpc-eio
              ];
              src = nix-filter.lib.filter {
                root = ./.;
                include = [ "dune-project" "examples" ];
              };
            };
            grpc = camlPkgs.buildDunePackage {
              pname = "grpc";
              version = "0.1.0";
              duneVersion = "3";
              nativeBuildInputs = with camlPkgs; [ mdx ];
              propagatedBuildInputs = with camlPkgs; [ ppxlib ];
              buildInputs = with camlPkgs; [ uri h2 ppx_deriving ];
              src = nix-filter.lib.filter {
                root = ./.;
                include = [ "dune-project" "lib/grpc" ];
              };
            };
            grpc-lwt = camlPkgs.buildDunePackage {
              pname = "grpc-lwt";
              version = "0.1.0";
              duneVersion = "3";
              buildInputs = with camlPkgs; [ self'.packages.grpc lwt ];
              src = nix-filter.lib.filter {
                root = ./.;
                include = [ "dune-project" "lib/grpc-lwt" ];
              };
            };
            grpc-async = camlPkgs.buildDunePackage {
              pname = "grpc-async";
              version = "0.1.0";
              duneVersion = "3";
              buildInputs = with camlPkgs; [ self'.packages.grpc async ];
              src = nix-filter.lib.filter {
                root = ./.;
                include = [ "dune-project" "lib/grpc-async" ];
              };
            };
            grpc-eio = camlPkgs.buildDunePackage {
              pname = "grpc-eio";
              version = "0.1.0";
              duneVersion = "3";
              buildInputs = with camlPkgs; [ self'.packages.grpc eio ];
              src = nix-filter.lib.filter {
                root = ./.;
                include = [ "dune-project" "lib/grpc-eio" ];
              };
            };
          };
          packages.default = self'.packages.grpc;
        };
    };
}
