{
module ProgramLexer where
}

-- Vamos a crear tokens a partir de un flujo de entrada
%wrapper "posn"

$digit  =   [0-9]    
@sconst =   [a-z][A-Za-z0-9_]* | \'($printable # \')+\'
@nconst =   (\-)? $digit+ (\.)? ($digit+)?
@var    =   [A-Z_][A-Za-z0-9_]*   

tokens :- 
    
    $white+             ;                               -- White space
    \/\*.*\*\/          ;                               -- Comment
    \%.*                ;                               -- Comment
    \.                  { \p s -> EoR p }               -- End of Rule
    \,                  { \p s -> Sep p }               -- Separator
    \;                  { \p s -> Sem p }               -- Semicolon
    \(                  { \p s -> LB p }                -- Left Bracket
    \)                  { \p s -> RB p }                -- Right Bracket
    ":-"                { \p s -> Imp p }               -- Implication
    \=                  { \p s -> Eq p }                -- Equal 
    @sconst             { \p s -> SConst (p, (filter (\c -> c /= '\'') s))} -- Symbolic Constant
    @nconst             { \p s -> NConst (p, (read s))} -- Numeric Constant
    @var                { \p s -> Var (p, s)}           -- Variable
    .                   { \p s -> Err p }               -- Error

{
-- Token type
data Token =
        EoR AlexPosn              |
        Sep AlexPosn              |
        Sem AlexPosn              |
        LB AlexPosn               |
        RB AlexPosn               |
        Imp AlexPosn              |
        Eq AlexPosn               |
        SConst (AlexPosn, String) |
        NConst (AlexPosn, Double) |
        Var (AlexPosn, String)    |
        Err AlexPosn             
        deriving (Show, Eq)

token_posn(EoR p) = p
token_posn(Sep p) = p
token_posn(Sem p) = p
token_posn(LB p) = p
token_posn(RB p) = p
token_posn(Imp p) = p
token_posn(Eq p) = p
token_posn(SConst (p, _)) = p
token_posn(NConst (p, _)) = p
token_posn(Var (p, _)) = p
token_posn(Err p) = p


prueba = do
    s <- getContents
    print (alexScanTokens s)
}