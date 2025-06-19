{
module Lexer (alexScanTokens) where
}

%wrapper "basic"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \-]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                     ;
  @id [$white]+ \"@content\"  { \s -> ("block", s) }
  L                           { \s -> ("left branch identifier", s) }
  R                           { \s -> ("right branch identifier", s) }
  \{                          { \s -> ("opening curly bracket", s) }
  \}                          { \s -> ("closing curly bracket", s) }
  @id                         { \s -> ("solo identifier", s) }
