{
  description = "A very functional flake";

  outputs = {
    self,
    nixpkgs,
  }: let
    systems = ["x86_64-linux"];

    config = system: let
      pkgs = import nixpkgs {inherit system;};
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          nil
          elmPackages.elm
          elmPackages.elm-language-server
          elmPackages.elm-format
          watchexec
        ];
      };

      formatter.${system} = pkgs.alejandra;
    };

    mergeSystems = acc: system:
      with import nixpkgs {inherit system;};
        lib.attrsets.recursiveUpdate acc (config system);
  in
    builtins.foldl' mergeSystems {} systems;
}
