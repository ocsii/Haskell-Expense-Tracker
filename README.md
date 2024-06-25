**Haskell Expense Tracker**

Running the code requires installing the following dependencies:
  1. pkgs.split
  2. pkgs.csv


To install the dependencies and run the code in replit, create a _replit.nix_ file under the _Config Files_ tab and build with the following code:

```

{ pkgs }: {
    deps = [
      pkgs.q
        (pkgs.haskellPackages.ghcWithPackages (pkgs: [
          # Put your dependencies here!
        pkgs.split pkgs.csv
        ]))
        pkgs.haskell-language-server
    ];
}

```
