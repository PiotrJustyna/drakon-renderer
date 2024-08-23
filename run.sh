#!/bin/sh

cabal run drakon-renderer -- \
    --textInputPath "./diagrams/drakon-diagram-3.json" \
    --textOutputPath "./diagrams/drakon-diagram-3-layout.json" \
    --svgOutputPath "./diagrams/drakon-diagram-3.svg"
