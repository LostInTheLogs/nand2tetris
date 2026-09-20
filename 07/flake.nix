{
  description = "Description for the project";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    opam-nix.url = "github:tweag/opam-nix";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";
    opam-nix.inputs.opam2json.inputs.nixpkgs.follows = "nixpkgs";
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };

  outputs = inputs @ {
    flake-parts,
    opam-nix,
    opam-repository,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        pkgs,
        self',
        system,
        ...
      }: let
        package = "hackvmt";
        on = opam-nix.lib.${system};
        repos = [opam-repository];
        devPackagesQuery = {
          # You can add "development" packages here. They will get added to the devShell automatically.
          ocaml-config = "*";
          ocaml-lsp-server = "*";
          ocamlformat = "*";
          menhir-lsp = "*";
          menhirformat = "*";
        };
        query =
          devPackagesQuery
          // {
            ## You can force versions of certain packages here, e.g:
            ## - force the ocaml compiler to be taken from opam-repository:
            # ocaml-base-compiler = "*";
            ## - or force the compiler to be taken from nixpkgs and be a certain version:
            # ocaml-system = "4.14.0";
            ## - or force ocamlfind to be a certain version:
            # ocamlfind = "1.9.2";
          };
        scope =
          on.buildOpamProject' {
            inherit repos;
            inherit pkgs;
          }
          ./.
          query;
        overlay = final: prev: {
          # You can add overrides here
          ${package} = prev.${package}.overrideAttrs (_: {
            # Prevent the ocaml dependencies from leaking into dependent environments
            doNixSupport = false;
          });
        };
        scope' = scope.overrideScope overlay;
        # The main package containing the executable
        main = scope'.${package};
        # Packages from devPackagesQuery
        devPackages = builtins.attrValues (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope');
      in {
        packages.default = main;

        devShells.default = pkgs.mkShell {
          inputsFrom = [main];
          buildInputs =
            devPackages
            ++ [
              # You can add packages from nixpkgs here
            ];
        };

        formatter = pkgs.alejandra;
      };
    };
}
