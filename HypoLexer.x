{
module HypoLexer where
}

-- Vamos a crear tokens a partir de un flujo de entrada
%wrapper "basic" 

$digit  =   [0-9]  
@nconst =   (\-)? $digit+ (\.)? ($digit+)?
@unit   =   second s? | minute s? | hour s? | day s? | week s? | month s? | year s? 

tokens :-

    $white+             ;                           -- White space
    \(                  { \s -> LB }                -- Paréntesis izquierdo
    \)                  { \s -> RB }                -- Paréntesis derecho
    \,                  { \s -> COMA }              -- Coma
    or                  { \s -> O}                  -- Disyunción
    approximately       { \s -> APROXIMADAMENTE }   -- Aproximadamente
    equal               { \s -> IGUAL }             -- Igualdad
    before              { \s -> ANTES }             -- Antes
    after               { \s -> DESPUES }           -- Despues
    more                { \s -> MAS }               -- Mas
    less                { \s -> MENOS }             -- Menos
    than                  { \s -> DE }                -- De
    few                 { \s -> POCO }              -- Poco
    many                { \s -> MUCHO }             -- Mucho
    @nconst             { \s -> CANTIDAD (read s) } -- Cantidad
    @unit               { \s -> UNIDAD s }          -- Unidad
    .                   { \s -> ERROR }             -- Error              

{
-- Token type
data Token =
    LB                  |
    RB                  |
    COMA                |
    O                   |
    APROXIMADAMENTE     |
    IGUAL               |
    ANTES               |
    DESPUES             |
    MAS                 |
    MENOS               |
    DE                  |
    POCO                |
    MUCHO               |
    CANTIDAD Double     |
    UNIDAD String       |
    ERROR     
    deriving (Show)
prueba = do
    s <- getContents
    print (alexScanTokens s)
}