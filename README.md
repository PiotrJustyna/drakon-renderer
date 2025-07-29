# drakon-renderer

Reasonably portable drakon-esque diagrams renderer. Development, compilation and execution are intended to take place in containers.

**Important:**

This is work in progress and, while progress is being made every week, the renderer is not yet guaraneed to render drakon-correct diagrams (we're getting close, though, I feel!). Current focus is to translate [the drakon whitepaper](https://drakon.su/_media/video_i_prezentacii/graphical_syntax_.pdf) into a Haskell-friendly set of types and functions.

## examples

Sample diagrams the renderer can produce today:

### primitive diagram 1

#### input

```
1 "custom content - action"
2 "custom content - fork"
  L {
    3 "custom content - action"
    4 "custom content - action"
    5 "custom content - action"
  }
  R {
    6 "custom content - action"
    7 "custom content - action"
    8 "custom content - fork"
    L {
      9 "custom content - action"
    }
    R {
      10 "custom content - action"
      11 "custom content - action"
    }
  }
```

#### output

![primitive-diagram-1](./diagrams/primitive-diagram-1.svg)

### primitive diagram 2

#### input

```
1 "custom content - fork"
  L {
    2 "custom content - action"
    3 "custom content - action"
  }
  R {
  }
```

#### output

![primitive-diagram-2](./diagrams/primitive-diagram-2.svg)

### primitive diagram 3

#### input

```
2 "custom content - fork"
  L {
    1 "custom content - action"
    3 "custom content - action"
    4 "custom content - action"
    5 "custom content - action"
  }
  R {
    8 "custom content - fork"
    L {
      9 "custom content - action"
    }
    R {
      11 "custom content - action"
    }
  }
```

#### output

![primitive-diagram-3](./diagrams/primitive-diagram-3.svg)

### silhouette diagram 1

#### input

```
1 {
  1a "find a bus stop"
  1b "has a bus arrived"
    L {
      1aa "passenders boarding"
    }
    R {
    }
  1c "is it your turn"
    L {
    }
    R {
      1ca "wait for your turn"
      1c
    }
  1d "is it possible to enter the bus"
    L {
      1da "enter the bus"
    }
    R {
    }
  2
}
2 {
  2a "action"
  3
}
3 {
  3a "action"
  4
}
4 {
  4a "action"
}
```

#### output

![silhouette-diagram-1](./diagrams/silhouette-diagram-1.svg)

## development environment

| command | description |
| --- | --- |
| `./start-development-environment.sh` | starts a fully dockerized development environment |
| `./build.sh` | builds and lints code - preferrably while development environment docker container is running, otherwise local installation of cabal and hlint is required) |
| `./run.sh` | runs code - just like the build script, the preferred way to use it is while the development environment is running |
| `./format.sh` | formats all `*.hs` files located in the `./app` directory using [hindent](https://github.com/mihaimaruseac/hindent) |
| `exit` | terminates development environment |

## community

* [youtube](https://www.youtube.com/playlist?list=PL9-WsOrOzOxSqWNqzhzyBGZsN0sOxEF6Q)

## resources

* [drakon whitepaper](https://drakon.su/_media/video_i_prezentacii/graphical_syntax_.pdf)

* [drakon wiki](https://en.m.wikipedia.org/wiki/DRAKON)

* [drakon.su](https://drakon.su/start)

* [drakon](https://drakonhub.com/read/docs)

* [diagrams](https://archives.haskell.org/projects.haskell.org/diagrams/doc/quickstart.html#introduction)

* [diagrams - user manual](https://archives.haskell.org/projects.haskell.org/diagrams/doc/manual.html)

* [colours](https://www.colourlovers.com)

* [colorkit](https://colorkit.co/)

  * [default palette](https://colorkit.co/palette/642915-963e20-c7522a-e5c185-fbf2c4-74a892-008585-006464-004343/)

* [alex](https://haskell-alex.readthedocs.io/en/latest/index.html)

* [lexing with alex](https://serokell.io/blog/lexing-with-alex#our-first-lexer)

* useful haskell modules:

  * [GHC.Data.Graph.Directed](https://hackage.haskell.org/package/ghc-9.4.7/docs/GHC-Data-Graph-Directed.html)

  * [GHC.Utils.Outputable](https://hackage.haskell.org/package/ghc-9.4.7/docs/GHC-Utils-Outputable.html)

* if you struggle with `.git` permissions, try:

  ```bash
  # Note: Use this command with caution as it changes file ownership. Only run it if you understand the security implications.
  sudo chown -R yourusername .git
  ```
