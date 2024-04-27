{
module ProgramParser where

import ProgramLexer
import Unificacion
import Prolog
import PROLogic
import ManejadorRedes
import NumerosBorrosos
import Exception
import ManejoTiempo
import HypoParser(parse)
import Relaciones

}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Ex } { thenE } { returnE }

%token 
    '.'     { EoR _ }
    ','     { Sep _ }
    ';'     { Sem _ }
    '('     { LB _ }
    ')'     { RB _ }
    ":-"    { Imp _ }
    '='     { Eq _ }
    sconst  { SConst $$ }
    nconst  { NConst $$ }
    var     { Var $$ }


%%

Inicio   : ProgramaAmp            { creaProgramaAmp (reverse $1) }

ProgramaAmp : ProgramaAmp '.' ClausulaAmp { $3 : $1 }
            | ProgramaAmp '.'             { $1 }
            | ClausulaAmp                 { [$1] }
            | {- empty -}                 { [] }

ClausulaAmp : Clausula ';' ClausulaRed    { creaClausulaAmp $1 $3 }
            | Clausula                    { creaClausulaAmp $1 universal}

ClausulaRed : Origen ',' Restricciones    { creaClausulaRed $1 (reverse $3) }
            | Restricciones               { creaClausulaRed origenUniversal (reverse $1)}

Origen : '(' Nodo ',' '(' sconst ',' sconst ',' sconst ',' sconst ')' ')'  { creaOrigen $2 (snd $5,snd $7,snd $9,snd $11) }

Restricciones : Restricciones ',' Restriccion { $3 : $1 }
              | Restriccion                   { [$1] }
              | {- empty -}                   { [] }

Restriccion : '(' Nodo ',' Nodo ',' '(' nconst ',' nconst ',' nconst ',' nconst ')' sconst ')' { ($2,$4,(snd $7,snd $9,snd $11,snd $13),strToUnidad (snd $15)) }
            | '(' Nodo ',' Nodo ',' sconst ')' {case (procesaRelacion $2 (snd $6) $4) of
                                                         (Ok r) -> r}

Nodo : NombreNodo '=' ValorNodo       {creaNodoVal $1 $3}
     | NombreNodo                     {creaNodo $1}

NombreNodo : var        {snd $1}
           | sconst     {snd $1}

ValorNodo : sconst      {snd $1}
          | nconst      {show (snd $1)}

Clausula : Atomo ":-" Atomos      { ($1, reverse $3) }
         | Atomo                  { ($1, []) }

Atomos   : Atomos ',' Atomo       { $3 : $1 }
         | Atomo                  { [$1] }

Atomo    : sconst '(' Terms ')'   { A (snd $1) (reverse $3) }
         | sconst                 { A (snd $1) [] }
         | Termino '=' Termino    { I $1 $3 }

Terms    : Terms ',' Termino      { $3 : $1 }
         | Termino                { [$1] }
         | {- empty -}            { [] }

Termino  : var                    { V (snd $1) }
         | sconst                 { T (snd $1) []}
         | nconst                 { T (show (snd $1)) []}
         | sconst '(' Terms ')'   { T (snd $1) (reverse $3) }

{
type Inicio = ProgramaAmp
type Restricciones = [Restriccion]
type Restriccion = (Nodo,Nodo,NumeroBorroso,UnidadTiempo)
type NombreNodo = String
type ValorNodo = String
type Atomos = [Atomo]
type Terms  = [Termino]

procesaRelacion :: Nodo -> String -> Nodo -> Ex (Nodo,Nodo,NumeroBorroso,UnidadTiempo)
procesaRelacion n1 rel n2 =
    case relacion of
        (Ok r) -> Ok (n1,n2,getDist r,getUT r)
        (Failed err) -> Failed err
    where
        relacion = HypoParser.parse rel

lexer :: String -> [Token]
lexer = alexScanTokens

--prueba file = readFile file >>= print . parser . lexer

parse :: String -> Ex ProgramaAmp
parse = parser . lexer 

-- Errores
parseError :: [Token] -> Ex a
parseError [] = failE "No token found."
parseError (t:tks) = failE ("Parse error: at line " ++ (show line) ++ ", column " ++ (show column) ++ ".")
    where 
        AlexPn _ line column = token_posn t;
}  


