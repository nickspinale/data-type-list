{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "data-type-util";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
