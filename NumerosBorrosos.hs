{-# OPTIONS_GHC -fno-warn-tabs #-} 

module NumerosBorrosos where

import Segmentos

-- Definición de número borroso como una 4-tupla que representa una
-- distribución trapezoidal
type NumeroBorroso = (Double,Double,Double,Double)

-- Representa una distribución en el infinito
infinito :: NumeroBorroso
infinito = (-1/0,-1/0,1/0,1/0)

-- Devuelve el opuesto de un número borroso
opuestoBorroso :: NumeroBorroso -> NumeroBorroso
opuestoBorroso (a,b,c,d) = (-d,-c,-b,-a)

-- Suma binaria de números borrosos con distribucion trapezoidal
sumaBorrosa :: NumeroBorroso -> NumeroBorroso -> NumeroBorroso
sumaBorrosa (a1,c1,d1,b1) (a2,c2,d2,b2) 
    | (a1,c1,d1,b1) == infinito || (a2,c2,d2,b2) == infinito = infinito
    | otherwise = (a1+a2,c1+c2,d1+d2,b1+b2)

-- Intersección borrosa para distribuciones trapezoidales
interseccionBorrosa :: NumeroBorroso -> NumeroBorroso -> NumeroBorroso
interseccionBorrosa (a1,c1,d1,b1) (a2,c2,d2,b2) 
    | (a1,c1,d1,b1) == infinito = (a2,c2,d2,b2)
    | (a2,c2,d2,b2) == infinito = (a1,c1,d1,b1)
    | otherwise = (max a1 a2, max c1 c2, min d1 d2, min b1 b2)

-- Unión borrosa para distribuciones trapezoidales
unionBorrosa :: NumeroBorroso -> NumeroBorroso -> NumeroBorroso
unionBorrosa (a1,c1,d1,b1) (a2,c2,d2,b2) 
    | (a1,c1,d1,b1) == infinito = (a2,c2,d2,b2)
    | (a2,c2,d2,b2) == infinito = (a1,c1,d1,b1)
    | otherwise = (min a1 a2, min c1 c2, max d1 d2, max b1 b2)

-- Escala según un factor
escala :: NumeroBorroso -> Double -> NumeroBorroso
escala (a,b,c,d) f
	| (a,b,c,d) == infinito = infinito
	| f >= 0 = (f*a,f*b,f*c,f*d)
	| otherwise = (f*d,f*c,f*b,f*a)

-- Defuzzifica un número borroso
defuzzy :: NumeroBorroso -> Double
defuzzy (a,b,c,d)
	| (a,b,c,d) == infinito = 1/0
	| otherwise = (0.5 * c) + ((1-0.5) * b)

-- Diremos que un número borroso es "inconsistente" cuando no representa un trapecio
esConsistente :: NumeroBorroso -> Bool
esConsistente (a,b,c,d) = a <= b && b <= c && c <= d


-- Operacion maxmin
maxmin :: NumeroBorroso -> NumeroBorroso -> (Bool,Bool) -> Double
maxmin (a1,b1,c1,d1) (a2,b2,c2,d2) (inv1,inv2)
	| xor inv1 inv2 && a1 < b2 && a2 < b1 && c2 < d1 && c1 < d2 = max izq1_izq2 der1_der2
	| not inv1 && inv2 && a1 < b2 && a2 < b1 && d1 <= c2 = izq1_izq2
	| inv1 && not inv2 && a1 < b2 && a2 < b1 && d2 <= c1 = izq1_izq2
	| not inv1 && inv2 && c2 < d1 && c1 < d2 && b2 <= a1 = der1_der2
	| inv1 && not inv2 && c2 < d1 && c1 < d2 && b1 <= a2 = der1_der2
	| not inv1 && inv2 && d1 <= c2 && b2 <= a1 = 0
	| inv1 && not inv2 && d2 <= c1 && b1 <= a2 = 0
	| inv1 || inv2 = 1
	| a2 < d1 && c1 < b2 = der1_izq2
	| a1 < d2 && c2 < b1 = izq1_der2
	| d1 < a2 || d2 < a1 = 0
    | d1 == a2 && (c1 < d1 || a2 < b2) = 0
	| d2 == a1 && (c2 < d2 || a1 < b1) = 0
	| otherwise = 1
		where
			xor a b = (a || b) && not (a && b) 
			izq1 = if inv1 then ((a1,1),(b1,0))
					else ((a1,0),(b1,1)) 
			der1 = if inv1 then ((c1,0),(d1,1))
					else ((c1,1),(d1,0))
			izq2 = if inv2 then ((a2,1),(b2,0))
					else ((a2,0),(b2,1)) 
			der2 = if inv2 then ((c2,0),(d2,1))
					else ((c2,1),(d2,0))
			izq1_izq2 = snd (interseccion izq1 izq2)
			izq1_der2 = snd (interseccion izq1 der2)
			der1_der2 = snd (interseccion der1 der2)
			der1_izq2 = snd (interseccion der1 izq2) 

-- Grado de posibilidad
posibilidad :: NumeroBorroso -> NumeroBorroso -> Double
posibilidad nb1 nb2 = maxmin nb1 nb2 (False,False)

-- Grado de necesidad
necesidad :: NumeroBorroso -> NumeroBorroso -> Double
necesidad nb1 nb2 =
	-- | nb1 == nb2 = 1
	-- | otherwise = 1 - (maxmin nb1 nb2 (False,True))
	1 - (maxmin nb1 nb2 (False,True))
