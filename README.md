# drakon-renderer

## development environment

* start - `./start.sh`
* exit - `exit`

## hello world

* `cabal update`
* `cabal install --lib diagrams`
* `cabal install --lib diagrams-lib`
* `cabal install --lib diagrams-svg`
* `cabal install --lib base`
* `ghc HelloWorld.lhs` 
* `./HelloWorld -o hello-world.svg -w 400`

![hello-world](./hello-world.svg)

## resources

* [literate programming](https://wiki.haskell.org/Literate_programming)
* [drakon](https://drakonhub.com/read/docs)
* [diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html#introduction)
* diagrams operators:
  * `(#)` - `x # f = f x`
  * `(|||)` - combine two diagrams horizontally
  * `hcat` - combine a list of diagrams horizontally
  * `===` - combine two diagrams vertically
  * `vcat` - combine a list of diagrams vertically
  * `atop` - combine diagrams atop each other