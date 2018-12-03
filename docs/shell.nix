with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  nativeBuildInputs = [ imagemagick ];
  buildInputs = [ haskellPackages.lhs2tex
                  (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      stmaryrd kpfonts geometry hyperref

                      todonotes

                      amsmath mathtools

                      colortbl polytable lazylist
                      fancyvrb

                      #graphicx
                      pstricks

                      # build tools
                      latexmk

                      ;
                  })
                ];
}

