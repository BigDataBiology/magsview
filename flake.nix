{
  description = "MAGs view";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/ab8b04cdce6a103c76cac8966fbfa9de37823653";
  outputs = { self, nixpkgs }:
  let system = "x86_64-linux";
  in {

        packages.x86_64-linux.website = (import ./default.nix) { inherit nixpkgs system; };

        defaultPackage.x86_64-linux = self.packages.x86_64-linux.website;
      };
}
