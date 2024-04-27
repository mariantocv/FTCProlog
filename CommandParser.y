{
module CommandParser where

import CommandLexer
import Unificacion
import Prolog
import PROLogic
import ManejadorRedes
import NumerosBorrosos
import ImpComandos
import Exception
import ManejoTiempo

}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Ex } { thenE } { returnE }

%token
    '.'      { EoR }
    ','      { Sep }
    ';'      { Sem }
    ':'      { Points }
    '('      { LB }
    ')'      { RB }
    '='      { Eq }
    comopt   { ComOpt $$ }
    sconst   { SConst $$ }
    nconst   { NConst $$ }
    var      { Var $$ }

%nonassoc sconst nconst
%nonassoc '.'
%nonassoc ':'
%nonassoc ';'
%nonassoc ','

%%


Consulta : sconst Opciones Argumentos ':' ObjetivoAmp '.'  { CO $1 (reverse $2) (reverse $3) $5 }
         | sconst Opciones Argumentos '.'                  { C $1 (reverse $2) (reverse $3) }

Opciones : Opciones Opcion                          { $2 : $1 }
           | Opcion                                 { [$1] }
           | {- empty -}                            { [] }

Opcion : comopt                                     { $1 }

Argumentos : Argumentos Argumento                   { $2 : $1 }
           | Argumento                              { [$1] }
           | {- empty -}                            { [] }

Argumento : sconst                                  { $1 }

ObjetivoAmp : Objetivo ';' ClausulaRed      { creaObjetivoAmp (reverse $1) $3 }
            | Objetivo                      { creaObjetivoAmp (reverse $1) universal }

Objetivo : Objetivo ',' Atomo     { $3 : $1 }
         | Atomo                  { [$1] }

Atomo    : sconst '(' Terms ')'   { A $1 (reverse $3) }
         | sconst                 { A $1 [] }
         | Termino '=' Termino    { I $1 $3 }

Terms    : Terms ',' Termino      { $3 : $1 }
         | Termino                { [$1] }
         | {- empty -}            { [] }

Termino  : var                    { V $1 }
         | sconst                 { T $1 []}
         | nconst                 { T (show $1) []}
         | sconst '(' Terms ')'   { T $1 (reverse $3) }

ClausulaRed : Origen ',' Restricciones  { creaClausulaRed $1 (reverse $3) }     
            | Restricciones             { creaClausulaRed origenUniversal (reverse $1) }

Origen : '(' Nodo ',' '(' sconst ',' sconst ',' sconst ',' sconst ')' ')'  { creaOrigen $2 ($5,$7,$9,$11) }

Restricciones : Restricciones ',' Restriccion { $3 : $1 }
              | Restriccion                   { [$1] }
              | {- empty -}                   { [] }

Restriccion : '(' Nodo ',' Nodo ',' '(' nconst ',' nconst ',' nconst ',' nconst ')' sconst ')' { ($2,$4,($7,$9,$11,$13),strToUnidad $15) }

Nodo : NombreNodo '=' ValorNodo       {creaNodoVal $1 $3}
     | NombreNodo                     {creaNodo $1}

NombreNodo : var        {$1}
           | sconst     {$1}

ValorNodo : sconst      {$1}
          | nconst      {show ($1)}


{
type Restricciones = [Restriccion]
type Restriccion = (Nodo,Nodo,NumeroBorroso,UnidadTiempo)
type NombreNodo = String
type ValorNodo = String
type Atomos = [Atomo]
type Terms  = [Termino]

lexer :: String -> [Token]
lexer = alexScanTokens

--prueba = getContents >>= print . parser . lexer

parse :: String -> Ex Consulta
parse = parser . lexer 


-- Errores
parseError :: [Token] -> Ex a
parseError _ = failE "Parse error"
}  


