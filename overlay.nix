self: super:
let inherit (super) fetchFromGitHub;
in {
  ocaml-ng = super.ocaml-ng // {
    ocamlPackages_5_1 = super.ocaml-ng.ocamlPackages_5_1.overrideScope'
      (oself: super: {
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

