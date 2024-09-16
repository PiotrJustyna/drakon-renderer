#!/bin/sh

#cabal run drakon-renderer -- \
#    --textInputPath "./diagrams/real-life-diagram-1.json" \
#    --textOutputPath "./diagrams/real-life-diagram-1-layout.json" \
#    --svgOutputPath "./diagrams/real-life-diagram-1.svg"

cabal run drakon-renderer -- \
    --textInputPath "./diagrams/drakon-diagram.json" \
    --textOutputPath "./diagrams/drakon-diagram-layout.json" \
    --svgOutputPath "./diagrams/drakon-diagram.svg"
