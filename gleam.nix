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
  version = "1.0.0-rc2";

  src = fetchFromGitHub {
    owner = "gleam-lang";
    repo = pname;
    rev = "refs/tags/v${version}";
    hash = "sha256-WxdywmSn8U8Dth0T0guJrdU9w+sgyQLVsjyAM1Jjoa8=";
  };

  nativeBuildInputs = [ git pkg-config ];

  buildInputs = [ openssl ];

  cargoHash = "sha256-NsIyP2vTzuyiz8UmparUI7FXLyt7CTNBBoa3q9OWujE=";

  passthru.updateScript = nix-update-script { };
}
