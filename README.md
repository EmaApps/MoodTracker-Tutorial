# Mood Tracker Tutorial

Source for the Ema tutorial: https://ema.srid.ca/tutorial

## Getting Started

To develop with full IDE support in Visual Studio Code, follow these steps:

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Setup the [garnix cache](https://garnix.io/docs/caching) (Nix binary cache). This is optional if you are okay with compiling for hours.
- Run `nix-shell --run haskell-language-server` to sanity check your environment 
- Open the repository [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - Install the recommended extensions
    - <kbd>Ctrl+Shift+P</kbd> to run the command "Nix-Env: Select Environment" and select `shell.nix`. The extension will ask you to reload VSCode at the end.
- Press <kbd>Ctrl+Shift+B</kbd> in VSCode, or run `bin/run` in terminal, to launch the Ema dev server, and navigate to http://localhost:9001/

All but the final step need to be done only once. Check [the Ema tutorial](https://ema.srid.ca/start/tutorial) next.

## Note

- We are using GHC 9.2 which is not yet the default in `nixpkgs`, so you may want to use the [garnix cache](https://garnix.io/docs/caching) to avoid long compilation.
- This project uses [relude](https://github.com/kowainik/relude) as its prelude, as well as Tailwind+Blaze as CSS utility and HTML DSL. Even though the author highly recommends them, you are of course free to swap them out for the library of your choice.
  - Tailwind CSS is compiled, alongside Ghcid, via foreman (see `./Procfile`)
- As a first step to using this template, 
  - change the project name in .cabal, flake.nix and hie.yaml files; then commit changes to Git.
      - To automate this, `mv MoodTracker-Tutorial.cabal myproject.cabal; nix run nixpkgs#sd -- MoodTracker-Tutorial myproject * */* .github/*/*`
- Configuration:
  - To change the port (or the Ema CLI arguments, used by `bin/run`), see `./.ghcid` (if you leave out `--port` a random port will be used)
  - To update Ema to the latest Git revision, run `nix flake lock --update-input ema` or just `nix flake update` (the latter updates all Nix inputs)
    - Be sure to check https://ema.srid.ca/guide/upgrade for changes needed.
  - To add/remove Haskell dependencies, see the .cabal file. If a dependency is unavailable in nixpkgs, you can override it (to point to say a Git repo) in the `source-overrides` (or `overrides` if you need more power) attribute of flake.nix. You can imitate the manner in which the `ema` package itself is overridden.
- To generate the site, run:
  ```sh
  mkdir ../output 
  nix run . -- gen ../output
  ```
  - You might want to change or remove the `<base>` tag in `Main.hs` depending on where you will be deploying the site.

## Non-Nix workflow

To use this repository without Nix, such as with plain Cabal or Stack, you need to have the following installed manually:

- ghcid (used by `bin/run-haskell` which `./Procfile` invokes)
- [tailwind runner](https://hackage.haskell.org/package/tailwind) along with [tailwind CLI](https://tailwindcss.com/docs/installation)
- [foreman](http://ddollar.github.io/foreman/) (or one of its rewrites)

Once all the above are installed, run `foreman start` to start the Ema live server.
