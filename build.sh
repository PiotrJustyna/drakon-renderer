# cabal build && hlint .
alex  --ghc ./app/Drakon/Lexer.x \
      --outfile="./app/Drakon/Lexer.hs" \
      --info="./app/Drakon/Lexer.info" && \
happy --ghc ./app/Drakon/Parser.y \
      --outfile="./app/Drakon/Parser.hs" \
      --info="./app/Drakon/Parser.info" && \
cabal build .
