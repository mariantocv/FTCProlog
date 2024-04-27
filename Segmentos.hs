module Segmentos where

type Punto = (Double,Double)
type Vector = (Double,Double)
type Segmento = (Punto,Punto)

-- Devuelve el vector de un segmento
vector :: Segmento -> Vector
vector ((a,b),(c,d)) = (c-a,d-b)

-- Suma un vector a un punto
sumaVector :: Punto -> Vector -> Punto
sumaVector (p1,p2) (v1,v2) = (p1+v1,p2+v2) 

{- 
(x1,x2)+a(u1,u2) = (y1,y2)+b(v1,v2)

x1 + a * u1 = y1 + b * v1
x2 + a * u2 = y2 + b * v2

a = (y1 - x1 + b * v1)/u1 ->
-> (x2 * u1 + y1 * u2 - x1 * u2 + b * v1 * u2)/u1 = y2 + b * v2 ->
->  x2 * u1 + y1 * u2 - x1 * u2 + b * v1 * u2 = y2 * u1 + b * v2 * u1 ->
-> b * (v1 * u2 - v2 * u1) = u1 * (y2 - x2) - u2 * (y1 - x1) ->
-> b = (u1 * (y2 - x2) - u2 * (y1 - x1))/(v1 * u2 - v2 * u1)

-}

-- Devuelve el punto intersección entre 2 segmentos
interseccion :: Segmento -> Segmento -> Punto
interseccion (p1,q1) (p2,q2) = sumaVector p2 (b*v1,b*v2)
    where
        (u1,u2) = vector (p1,q1);
        (v1,v2) = vector (p2,q2);
        (x1,x2) = p1;
        (y1,y2) = p2;
        b = (u1 * (y2 - x2) - u2 * (y1 - x1))/(v1 * u2 - v2 * u1)
