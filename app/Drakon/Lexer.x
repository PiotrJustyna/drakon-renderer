{
module Main (main) where
}

%wrapper "basic"

words :-

$white+   { \s -> "here is your whitespace!" }
[0-9]+    { \s -> "here is your number!" }
[A-Za-z]+	{ \s -> "here is your token!" }

{
main = do
 s <- getContents
 print (length (alexScanTokens s))
}
