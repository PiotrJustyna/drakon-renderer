#!/bin/sh

cabal run drakon-renderer -- \
    --input "./diagrams/drakon-diagram-3.json" \
    --output "./diagrams/drakon-diagram-3-layout.json"