{
module Lexer
  (alexScanTokens,
  Token(TokenBlock,
  TokenLeftBranchIdentifier,
  TokenRightBranchIdentifier,
  TokenSoloIdentifier,
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

  $white+                     ;
  @id [$white]+ \"@content\"  { \s -> TokenBlock s }
  L                           { \s -> TokenLeftBranchIdentifier s }
  R                           { \s -> TokenRightBranchIdentifier s }
  @id                         { \s -> TokenSoloIdentifier s }
  \{                          { \_ -> TokenOCB }
  \}                          { \_ -> TokenCCB }

{
data Token
  = TokenBlock String
  | TokenLeftBranchIdentifier String
  | TokenRightBranchIdentifier String
  | TokenSoloIdentifier String
  | TokenOCB
  | TokenCCB
  deriving Show
}
