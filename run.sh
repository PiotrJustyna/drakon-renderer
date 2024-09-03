#!/bin/sh

cabal run drakon-renderer -- \
    --textInputPath "./diagrams/drakon-diagram-4.json" \
    --textOutputPath "./diagrams/drakon-diagram-4-layout.json" \
    --svgOutputPath "./diagrams/drakon-diagram-4.svg"
