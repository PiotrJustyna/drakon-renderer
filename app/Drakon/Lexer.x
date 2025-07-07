{
module Lexer
  (alexScanTokens,
  Token(TokenBlock,
  TokenSoloIdentifier,
  TokenLeftBranch,
  TokenRightBranch,
  TokenOCB,
  TokenCCB)) where
}

%wrapper "basic"

$digit        = 0-9
$alpha        = [a-zA-Z]

$idChar       = [$alpha $digit \']
$contentChar  = [$alpha $digit $white \' \, \! \-]

@id           = $idChar+
@content      = $contentChar+

tokens :-

  $white+                         ;
  @id [$white]+ \"@content\"      { \s -> TokenBlock s }
  L                               { \_ -> TokenLeftBranch }
  R                               { \_ -> TokenRightBranch }
  @id                             { \s -> TokenSoloIdentifier s }
  \{                              { \_ -> TokenOCB }
  \}                              { \_ -> TokenCCB }

{
data Token
  = TokenBlock String
  | TokenSoloIdentifier String
  | TokenLeftBranch
  | TokenRightBranch
  | TokenOCB
  | TokenCCB
  deriving Show
}
