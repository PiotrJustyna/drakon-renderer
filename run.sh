cabal run drakon-renderer -- \
    --inputPath "./diagrams/simple-diagram-2.json" \
    --layoutOutputPath "./diagrams/simple-diagram-2-layout.json" \
    --balancedPathsOutputPath "./diagrams/simple-diagram-2-balanced-paths.md" \
    --svgOutputPath "./diagrams/simple-diagram-2.svg"

# cabal run drakon-renderer -- \
#     --inputPath "./diagrams/real-life-diagram-1.json" \
#     --layoutOutputPath "./diagrams/real-life-diagram-1-layout.json" \
#     --balancedPathsOutputPath "./diagrams/real-life-diagram-1-balanced-paths.md" \
#     --svgOutputPath "./diagrams/real-life-diagram-1.svg"

# cabal run drakon-renderer -- \
#     --textInputPath "./diagrams/drakon-diagram.json" \
#     --textOutputPath "./diagrams/drakon-diagram-layout.json" \
#     --svgOutputPath "./diagrams/drakon-diagram.svg"

# cabal run drakon-renderer -- \
#     --textInputPath "./diagrams/primitive-diagram-1.json" \
#     --textOutputPath "./diagrams/primitive-diagram-1-layout.json" \
#     --svgOutputPath "./diagrams/primitive-diagram-1.svg"
