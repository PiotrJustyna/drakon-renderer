{
module Main (main) where
}

%wrapper "basic"

words :-

$white+ ;
[A-Za-z0-9\'\-]+	{ \s -> "word: " <> s }

{
  main = do
    s <- getContents
    print (length (alexScanTokens s))
}
