{
  nixConfig.bash-prompt-suffix = "\[nix\] ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };


  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.rust-overlay.overlays.default
        ];
      };

      rust-collection = pkgs.rust-bin.nightly.latest.default.override {
        extensions = [ "miri-preview" "rust-src" ];
      };

      packages = with pkgs; [
        rust-collection
      ];
    in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = packages;
        };
      }
    );
}
