{ mkDerivation, base, base16-bytestring, binary, bytestring
, containers, pretty, stdenv
}:
mkDerivation {
  pname = "data-bitcode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring binary bytestring containers pretty
  ];
  homepage = "https://github.com/angerman/data-bitcode#readme";
  description = "bitcode reader and writer";
  license = stdenv.lib.licenses.bsd3;
}
