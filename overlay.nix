self: super: let
  inherit (super) fetchFromGitHub;
in {
  ocaml-ng =
    super.ocaml-ng
    // {
      ocamlPackages_5_1 =
        super.ocaml-ng.ocamlPackages_5_1.overrideScope'
        (oself: super:
          {
            h2 = super.h2.overrideAttrs (_: {
              src = fetchFromGitHub {
                owner = "dialohq";
                repo = "ocaml-h2";
                rev = "fc872de80a4d64725cd0651ab21399676be9de39";
                sha256 = "sha256-4FmTkc3sCHzuzDJoYtiLnsewXc8sjWS7FgcOJJTZ5fk=";
                fetchSubmodules = true;
              };
            });
            h2-eio = super.h2-eio.overrideAttrs (_: {
              src = fetchFromGitHub {
                owner = "dialohq";
                repo = "ocaml-h2";
                rev = "fc872de80a4d64725cd0651ab21399676be9de39";
                sha256 = "sha256-4FmTkc3sCHzuzDJoYtiLnsewXc8sjWS7FgcOJJTZ5fk=";
                fetchSubmodules = true;
              };
            });
            gluten-eio = super.gluten-eio.overrideAttrs (_: {
              src = fetchFromGitHub {
                owner = "dialohq";
                repo = "gluten";
                rev = "94f64daa376a6c860b0af7cdfb9daea54ec939b0";
                sha256 = "sha256-nJp+BLfqzjgnY9Mamrgbj2q5KDip8i3EMpjYL+ntV2w=";
                fetchSubmodules = true;
              };
            });
          });
    };
}

