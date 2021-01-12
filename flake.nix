{
  description = "A modular gRPC library";

  outputs = { self, nixpkgs }:
    with import nixpkgs { system = "x86_64-linux"; };
    let
      h2-src = fetchFromGitHub {
        owner = "anmonteiro";
        repo = "ocaml-h2";
        rev = "36bd7bfa46fb0eb2bce184413f663a46a5e0dd3b";
        sha256 = "sha256-8vsRpx0JVN6KHOVfKit6LhlQqGTO1ofRhfyDgJ7dGz0=";
      };

      hpack = ocamlPackages.buildDunePackage {
        pname = "hpack";
        version = "0.2.0";
        src = h2-src;
        useDune2 = true;
        buildInputs = (with ocamlPackages; [ angstrom faraday ]);
      };

      h2 = ocamlPackages.buildDunePackage {
        pname = "h2";
        version = "0.7.0";
        src = h2-src;
        useDune2 = true;
        buildInputs = (with ocamlPackages; [ hpack result httpaf psq base64 ]);
      };
    in
    {
      packages.x86_64-linux = rec {
        grpc =
          ocamlPackages.buildDunePackage {
            pname = "grpc";
            version = "0.1.0";
            src = self;
            useDune2 = true;
            doCheck = true;
            buildInputs = (with ocamlPackages; [ uri h2 ]);
          };

        grpc-lwt =
          ocamlPackages.buildDunePackage {
            pname = "grpc-lwt";
            version = "0.1.0";
            src = self;
            useDune2 = true;
            doCheck = true;
            buildInputs = (with ocamlPackages; [ ocaml-protoc lwt stringext h2 grpc ]);
          };
      };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.grpc;

      devShell.x86_64-linux = mkShell {
        buildInputs = [
          ocaml

          nixpkgs-fmt
          rnix-lsp
        ] ++ (with ocamlPackages; [ dune_2 ocaml-protoc uri ]);
      };
    };
}
