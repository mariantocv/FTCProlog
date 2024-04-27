{
module HypoParser where

import HypoLexer
import Exception
import Relaciones
import ManejoTiempo

}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Ex } { thenE } { returnE }

%token
    '('        { LB }
    ')'        { RB }
    ','        { COMA }
    o          { O }
    aprox      { APROXIMADAMENTE }
    igual      { IGUAL }
    antes      { ANTES }
    despues    { DESPUES }
    mas        { MAS }
    menos      { MENOS }
    de         { DE }
    poco       { POCO }
    mucho      { MUCHO }
    cantidad   { CANTIDAD $$ }
    unidad     { UNIDAD $$ }

%%

Relacion : Relacion o Rel           { uneRelacion $1 $3 }
         | Rel                      { $1 }

Rel : DistanciaTiempo          { $1 }
    | igual unidad             { al_mismo_tiempo (strToUnidad $2) }
    | aprox igual unidad       { aproximadamente (al_mismo_tiempo (strToUnidad $3)) }

DistanciaTiempo : DireccionTiempoRel                   { $1 }
                | ExtensionTiempo DireccionTiempoMod   { $2 $1 }

ExtensionTiempo : ExtensionTiempoAbsoluto                                    { $1 }
                | OperadorExpansion ExtensionTiempoAbsoluto                  { $1 $2 }
                | ExtensionTiempo OperadorExpansion ExtensionTiempoAbsoluto  { mezclaRelacion $1 ($2 $3) }

DireccionTiempoRel : antes   { antes segs }
                   | despues { despues segs }

DireccionTiempoMod : antes   { antesM }
                   | despues { despuesM }

OperadorExpansion : mas de    { mas_de }
                  | menos de  { menos_de }

ExtensionTiempoAbsoluto : CantidadTemporal          { $1 }
                        | aprox CantidadTemporal    { aproximadamente $2 }

CantidadTemporal : '(' cantidad ',' cantidad ',' cantidad ',' cantidad ')' unidad { cantidadBorrosa ($2,$4,$6,$8) (strToUnidad $10) } 
                 | cantidad unidad    { cantidad $1 (strToUnidad $2) }
                 | poco unidad        { poco (strToUnidad $2) }
                 | mucho unidad       { mucho (strToUnidad $2) }

{
type Rel = Relacion
type CantidadTemporal = Relacion
type ExtensionTiempoAbsoluto = Relacion
type OperadorExpansion = Modificador
type DireccionTiempoRel = Relacion
type DireccionTiempoMod = Modificador
type ExtensionTiempo = Relacion
type DistanciaTiempo = Relacion

segs = strToUnidad "seconds"


lexer :: String -> [Token]
lexer = alexScanTokens

--prueba = getContents >>= print . parser . lexer

parse :: String -> Ex Relacion
parse = parser . lexer 


-- Errores
parseError :: [Token] -> Ex a
parseError _ = failE "Parse error"
}  


