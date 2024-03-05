{ lib
, stdenv
, rustPlatform
, fetchFromGitHub
, git
, pkg-config
, openssl
, nix-update-script
}:

rustPlatform.buildRustPackage rec {
  pname = "gleam";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "gleam-lang";
    repo = pname;
    rev = "refs/tags/v${version}";
    hash = "sha256-gPlRihwK+J7s1SeymfVdVo/KIV+eEqxlLVOgsDWW9yo=";
  };

  nativeBuildInputs = [ git pkg-config ];

  buildInputs = [ openssl ];

  cargoHash = "sha256-ouu4Y1085dGSM7kGIWE+hBde6ZUOA1fO0AcHYXPOWzo=";

  passthru.updateScript = nix-update-script { };
}
