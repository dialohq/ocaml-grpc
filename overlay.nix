nix-filter: self: super: let
  inherit (super) fetchFromGitHub callPackage;
in {
  ocaml-ng =
    super.ocaml-ng
    // {
      ocamlPackages_5_1 =
        super.ocaml-ng.ocamlPackages_5_1.overrideScope'
        (oself: super: let
          ocamlProtocSrc = fetchFromGitHub {
            owner = "dialohq";
            repo = "ocaml-protoc";
            rev = "edf1ef344ef4b6968678ebe1852e2f0d1c564eb1";
            sha256 = "sha256-jCpxWSTxgDOZ3jNsHNs8/eLIovroUR/lQVcEqXg+je0=";
            fetchSubmodules = true;
          };
          hahaPkgsSrc = fetchFromGitHub {
            owner = "dialohq";
            repo = "haha";
            rev = "40cadfe0141d8032cab8408a68e7b1606e80a153";
            sha256 = "sha256-SCTDbkWh2tAGYeIGbBg1BWkTJM4LhDp9f3z8EMABRaQ=";
          };
          mkHahaPkg = pname: buildDeps:
            super.buildDunePackage {
              inherit pname;
              version = "0.0.1";
              duneVersion = "3";
              src = hahaPkgsSrc;
              nativeBuildInputs = [self.git];
              propagatedBuildInputs = buildDeps;
            };
        in {
          hpackv = mkHahaPkg "hpackv" (with oself; [angstrom faraday]);
          haha = mkHahaPkg "haha" (with oself; [eio_main angstrom faraday hpackv]);
          h2 = super.h2.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "dialohq";
              repo = "ocaml-h2";
              rev = "5fc0a4976ed25248872bac487ba344ebcaa76de0";
              sha256 = "sha256-SZKv6Cv45hRrM1e/P7bmmWT96IERmF41wUvyaQGHj3g=";
              fetchSubmodules = true;
            };
          });
          h2-eio = super.h2-eio.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "dialohq";
              repo = "ocaml-h2";
              rev = "5fc0a4976ed25248872bac487ba344ebcaa76de0";
              sha256 = "sha256-SZKv6Cv45hRrM1e/P7bmmWT96IERmF41wUvyaQGHj3g=";

              fetchSubmodules = true;
            };
          });
          pbrt = super.pbrt.overrideAttrs (_: {src = ocamlProtocSrc;});
          pbrt_services = super.buildDunePackage {
            pname = "pbrt_services";
            version = "3.0.1";
            duneVersion = "3";
            propagatedBuildInputs = [oself.pbrt oself.pbrt_yojson];
            src = ocamlProtocSrc;
          };
          pbrt_yojson = super.buildDunePackage {
            pname = "pbrt_yojson";
            version = "3.0.1";
            duneVersion = "3";
            propagatedBuildInputs = [super.yojson super.base64];
            src = ocamlProtocSrc;
          };
          ocaml-protoc = super.ocaml-protoc.overrideAttrs (_: {
            propagatedBuildInputs =
              super.ocaml-protoc.propagatedBuildInputs
              ++ [oself.pbrt_yojson oself.pbrt_services];
            src = ocamlProtocSrc;
          });
          gluten-eio = super.gluten-eio.overrideAttrs (_: {
            src = fetchFromGitHub {
              owner = "dialohq";
              repo = "gluten";
              rev = "e9ae4690ebd65b143e69955b1dc26ac77c25fa91";
              sha256 = "sha256-hT62/TWFD11Irn+fy43nNGB8PKF1UAux0i9+9U3a/Ho=";

              fetchSubmodules = true;
            };
          });
        });
    };
}
