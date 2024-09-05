#!/bin/sh

cabal run drakon-renderer -- \
    --textInputPath "./diagrams/drakon-diagram.json" \
    --textOutputPath "./diagrams/drakon-diagram-layout.json" \
    --svgOutputPath "./diagrams/drakon-diagram.svg"
