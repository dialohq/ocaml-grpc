self: super: let
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
            rev = "7a0160f13ba521249b8520d49cd8dad74da6cc90";
            sha256 = "sha256-j0puAN+/oYHjVL8mnRTtdJSEy8Jx7JrwNZ+gQ2hhcy4=";
          };
          eioPkgsSrc = fetchFromGitHub {
            owner = "dialohq";
            repo = "eio";
            rev = "8a9f8674764129a713a6c3d17c4fb82df4be393e";
            sha256 = "sha256-uo5bpfx9ZU2LNC5BbT862iYHmou8v7lURFRMRGkauzk=";
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
          eio = super.eio.overrideAttrs (_: {
            src = eioPkgsSrc;
          });
          eio_main = super.eio_main.overrideAttrs (_: {
            src = eioPkgsSrc;
          });
          hpackv = mkHahaPkg "hpackv" (with oself; [angstrom faraday]);
          haha = mkHahaPkg "haha" (with oself; [eio_main angstrom faraday hpackv]);
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
        });
    };
}
