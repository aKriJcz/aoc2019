# https://maybevoid.com/posts/2019-01-27-getting-started-haskell-nix.html
let pkgs = import <nixpkgs> {}; in
pkgs.mkShell {
  name = "advent";
  # this adds all the build inputs of your project package
  #inputsFrom = [ (import ./default.nix { inherit pkgs; };) ];
  # now add your other dependencies
  buildInputs = with pkgs.haskellPackages; [ (ghcWithPackages (pkgs: [ pkgs.vector ])) hdevtools ];
}
