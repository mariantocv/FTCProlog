{
module CommandLexer where
}

-- Vamos a crear tokens a partir de un flujo de entrada
%wrapper "basic"

$digit  =   [0-9]   
@comopt =   \-[a-z]+
@sconst =   [a-z][A-Za-z0-9_]* | \'($printable # \')+\'
@nconst =   (\-)? $digit+ (\.)? ($digit+)?
@var    =   [A-Z_][A-Za-z0-9_]*


tokens :-
    
    $white+             ;                           -- White space
    \.                  { \s -> EoR }               -- End of Rule
    \,                  { \s -> Sep }               -- Separator
    \:                  { \s -> Points }            -- Points
    \;                  { \s -> Sem }               -- Semicolon
    \(                  { \s -> LB }                -- Left Bracket
    \)                  { \s -> RB }                -- Right Bracket
    \=                  { \s -> Eq }                -- Equal
    @comopt             { \s -> ComOpt (tail s) }   -- Command Option
    @sconst             { \s -> SConst (filter (\c -> c /= '\'') s)} -- Smbolic Constant 
    @nconst             { \s -> NConst (read s)}    -- Numeric Constant
    @var                { \s -> Var s}              -- Variable
    .                   { \s -> Err }               -- Error

{
-- Token type
data Token =
        EoR             |
        Sep             |
        Points          |
        Sem             |
        LB              |
        RB              |
        Eq              |
        ComOpt String   |
        SConst String   |
        NConst Double   |
        Var String      |
        Err
        deriving (Show, Eq)

prueba = do
    s <- getContents
    print (alexScanTokens s)
}