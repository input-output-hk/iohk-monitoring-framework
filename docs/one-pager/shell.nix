with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "docsEnv";
  nativeBuildInputs = [ imagemagick ];
  buildInputs = [ haskellPackages.lhs2tex
                  (texlive.combine {
                    inherit (texlive)
                      amsmath
                      colortbl
                      geometry
                      fancyvrb
                      hyperref
                      kpfonts
                      latexmk
                      lazylist
                      mathtools
                      mdframed
                      needspace
                      polytable
                      pstricks
                      scheme-small
                      stmaryrd
                      todonotes
		                  wrapfig
                      ;
                  })
                ];
}

