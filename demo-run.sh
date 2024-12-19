cabal run drakon-renderer -- \
    --inputPath "./diagrams/simple-diagram-1/simple-diagram-1.json" \
    --layoutOutputPath "./diagrams/simple-diagram-1/simple-diagram-1-layout.json" \
    --balancedPathsOutputPath "./diagrams/simple-diagram-1/simple-diagram-1-balanced-paths.md" \
    --svgOutputPath "./diagrams/simple-diagram-1/simple-diagram-1.svg"

cabal run drakon-renderer -- \
    --inputPath "./diagrams/simple-diagram-2/simple-diagram-2.json" \
    --layoutOutputPath "./diagrams/simple-diagram-2/simple-diagram-2-layout.json" \
    --balancedPathsOutputPath "./diagrams/simple-diagram-2/simple-diagram-2-balanced-paths.md" \
    --svgOutputPath "./diagrams/simple-diagram-2/simple-diagram-2.svg"

cabal run drakon-renderer -- \
    --inputPath "./diagrams/simple-diagram-3/simple-diagram-3.json" \
    --layoutOutputPath "./diagrams/simple-diagram-3/simple-diagram-3-layout.json" \
    --balancedPathsOutputPath "./diagrams/simple-diagram-3/simple-diagram-3-balanced-paths.md" \
    --svgOutputPath "./diagrams/simple-diagram-3/simple-diagram-3.svg"

cabal run drakon-renderer -- \
    --inputPath "./diagrams/primitive-diagram-1/primitive-diagram-1.json" \
    --layoutOutputPath "./diagrams/primitive-diagram-1/primitive-diagram-1-layout.json" \
    --balancedPathsOutputPath "./diagrams/primitive-diagram-1/primitive-diagram-1-balanced-paths.md" \
    --svgOutputPath "./diagrams/primitive-diagram-1/primitive-diagram-1.svg"

cabal run drakon-renderer -- \
    --inputPath "./diagrams/drakon-diagram.json" \
    --layoutOutputPath "./diagrams/drakon-diagram-layout.json" \
    --balancedPathsOutputPath "./diagrams/drakon-diagram-balanced-paths.md" \
    --svgOutputPath "./diagrams/drakon-diagram.svg"
