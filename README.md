# drakon-renderer

Reasonably portable drakon diagrams renderer. Development, compilation and execution are intended to take place in containers.

## terminology

* `Title` - first step of a diagram
* `End` - last step of a diagram
* `Action` - "do X" non-branching action, basic building block of a diagram
* `Question` - branching step with two possible outcomes:
  * yes
  * no

[source 1](https://en.m.wikipedia.org/wiki/DRAKON#/media/File%3AIcons_of_Visual_Programming_Language_--DRAKON--.png)

[source 2](https://en.m.wikipedia.org/wiki/DRAKON)

## input syntax

```json
[
    {
        "iconDescription": "hello world process",
        "iconKind": "Title",
        "iconName": "1",
        "iconNamesOfDependentIcons": [
            "2",
            "3"
        ]
    },
    {
        "iconDescription": "Hello, world!",
        "iconKind": "Action",
        "iconName": "2",
        "iconNamesOfDependentIcons": [
            "3"
        ]
    },
    {
        "iconDescription": "end",
        "iconKind": "End",
        "iconName": "3",
        "iconNamesOfDependentIcons": []
    }
]
```

## output

As this is work in progress, at the time being the oputput is not svg but instead, it is a pretty printed directed graph resulting from deserializing input icons.

```
Vertices:
(Icon {iconName = "1", iconDescription = "hello world process", iconNamesOfDependentIcons = ["2","3"], iconKind = Title}, 1, [2, 3])
(Icon {iconName = "2", iconDescription = "Hello, world!", iconNamesOfDependentIcons = ["3"], iconKind = Action}, 2, [3])
(Icon {iconName = "3", iconDescription = "end", iconNamesOfDependentIcons = [], iconKind = End}, 3, [])
Edges:
(Icon {iconName = "1", iconDescription = "hello world process", iconNamesOfDependentIcons = ["2","3"], iconKind = Title}, 1, [2, 3]) -> (Icon {iconName = "2", iconDescription = "Hello, world!", iconNamesOfDependentIcons = ["3"], iconKind = Action}, 2, [3])
(Icon {iconName = "1", iconDescription = "hello world process", iconNamesOfDependentIcons = ["2","3"], iconKind = Title}, 1, [2, 3]) -> (Icon {iconName = "3", iconDescription = "end", iconNamesOfDependentIcons = [], iconKind = End}, 3, [])
(Icon {iconName = "2", iconDescription = "Hello, world!", iconNamesOfDependentIcons = ["3"], iconKind = Action}, 2, [3]) -> (Icon {iconName = "3", iconDescription = "end", iconNamesOfDependentIcons = [], iconKind = End}, 3, [])
```

## development environment

| command | description |
| --- | --- |
| `./start-development-environment.sh` | starts a fully dockerized development environment |
| `./build.sh` | builds and lints code - preferrably while development environment docker container is running, otherwise local installation of cabal and hlint is required) |
| `./run.sh` | runs code - just like the build script, the preferred way to use it is while the development environment is running |
| `exit` | terminates development environment |

## project status

You can see into the bigger ideas I have for the project (past, present, future) here: [dev-log.md](./dev-log.md).

## resources

* [drakon wiki](https://en.m.wikipedia.org/wiki/DRAKON)

* [drakon](https://drakonhub.com/read/docs)

* [diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html#introduction)

* [diagrams - user manual](https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html)

* [colours](https://www.colourlovers.com)

* [colorkit](https://colorkit.co/)

  * [default palette](https://colorkit.co/palette/642915-963e20-c7522a-e5c185-fbf2c4-74a892-008585-006464-004343/)

* useful haskell modules:

  * [GHC.Data.Graph.Directed](https://hackage.haskell.org/package/ghc-9.4.7/docs/GHC-Data-Graph-Directed.html)

  * [GHC.Utils.Outputable](https://hackage.haskell.org/package/ghc-9.4.7/docs/GHC-Utils-Outputable.html)
