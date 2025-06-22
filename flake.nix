{
  description = "A flake including some extra tools, but not GHC or Cabal.";

  # https://search.nixos.org/packages
  inputs = {
    nixpkgs.url = "flake:nixpkgs/25.05";
  };

  outputs = { self, nixpkgs }: 
  let system = "x86_64-linux" ;
  in {
        devShells.${system}.default = 
        let pkgs = nixpkgs.legacyPackages.${system};
        in 
            # https://ryantm.github.io/nixpkgs/builders/special/mkshell/
            pkgs.mkShell {
                packages = [
                    pkgs.graphviz 
                    pkgs.watchexec 
                    pkgs.python314
                    pkgs.ormolu
                ];
                # https://www.reddit.com/r/NixOS/comments/110xqki/comment/j8k996e/
                shellHook = 
                    ''
                      PS1='\e[4;32m(dev) \W\$\e[m ' 
                    '';
            };
    };
}