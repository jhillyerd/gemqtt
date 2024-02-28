{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};

      gleam-override = pkgs.callPackage ./gleam.nix {};
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          erlang
          gleam-override
          mosquitto
          rebar3
        ];
      };
    }
  );
}
