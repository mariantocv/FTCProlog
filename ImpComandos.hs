module ImpComandos where

import PROLogic
import ProgramParser as PP
import Exception
import Control.Exception
import Data.List
import Data.List.Split
import Data.Time
import ManejoTiempo
import HypoParser as HP
import Relaciones
import NumerosBorrosos
import ManejadorRedes

-- Tipo que representa una consulta
data Consulta = CO Comando Opciones Argumentos ObjetivoAmp | 
                C Comando Opciones Argumentos
                deriving (Show)

-- Tipo que representa el nombre de un comando
type Comando = String

-- Tipo que representa una opción (o muchas agrupadas en una sola) de un comando
type Opcion = String
type Opciones = [Opcion]

-- Junta todas las opciones en una sola
getOpciones :: [Opcion] -> Opcion
getOpciones os = nub (concat os)

-- Tipo que representa un 
type Argumento = String
type Argumentos = [Argumento]

-- Estado del programa
data Estado = E ProgramaAmp [Respuesta] | EFinal
    deriving (Eq)

estadoInicial :: Estado
estadoInicial = E programaVacio []

esFinal :: Estado -> Bool
esFinal es = es == EFinal

-- Tipos de las implementacions de comandos con y sin objetivo
type ConsultaObjetivoImp = (Estado -> Opciones -> Argumentos -> ObjetivoAmp -> IO Estado)
type ConsultaImp = (Estado -> Opciones -> Argumentos -> IO Estado)

-- Mapeo entre los comandos y sus implementaciones
type CorrespCOImp = (Comando, ConsultaObjetivoImp)
type CorrespCImp = (Comando, ConsultaImp)

consultasObjetivo :: [CorrespCOImp]
consultasObjetivo = [("c",consult)]

consultas :: [CorrespCImp]
consultas = [("help",help),
             ("load",load),
             ("n",next),
             ("q",quit),
             ("last",lastResult),
             ("resolv",resolv),
             ("time",time),
             ("succ",successor),
             ("pred",predecessor),
             ("firsts",firsts),
             ("lasts",lasts),
             ("hypo",hypoComp)]

getConsultaObjetivoImp :: Comando -> ConsultaObjetivoImp
getConsultaObjetivoImp com 
    | null imps = noCommandObjective com
    | otherwise = head imps
        where
            imps = [imp | (c,imp) <- consultasObjetivo, c==com]

getConsultaImp :: Comando -> ConsultaImp
getConsultaImp com 
    | null imps = noCommand com 
    | otherwise = head imps
        where
            imps = [imp | (c,imp) <- consultas, c==com]

-- Ejecuta la consulta realizada, en el contexto de un programa y la última respuesta obtenida
ejecuta :: Estado -> Consulta -> IO Estado
ejecuta es (CO com ps args obj) = ejecutaConsultaObjetivo es com ps args obj
ejecuta es (C com ps args) = ejecutaConsulta es com ps args

-- Ejecuta una consulta con objetivo
ejecutaConsultaObjetivo :: Estado -> Comando -> Opciones -> Argumentos -> ObjetivoAmp -> IO Estado
ejecutaConsultaObjetivo es com ps args obj = (getConsultaObjetivoImp com) es ps args obj

-- Ejecuta una consulta sin objetivo
ejecutaConsulta :: Estado -> Comando -> Opciones -> Argumentos -> IO Estado
ejecutaConsulta es com ps args = (getConsultaImp com) es ps args

----------------------------COMANDOS SIN OBJETIVO-------------------------------------
{- Comando vacío -}
noCommand :: Comando -> ConsultaImp
noCommand com es _ _ = 
    do 
        fail ("The \"" ++ com ++ "\"command does not exist.")
        return es

{- Muestra la forma de uso de los comandos indicados
    Los argumentos son nombres de comandos -}
help :: ConsultaImp
help es ps args
    | length args == 0 = fail "Use: help <comand1> [<comand2>...<comandN>]."
    | otherwise =
        do
            putStrLn (concat ["======================================================\n"++
                              (getHelp c)++
                              "\n\n"   
                              | c <- args])
            return es

getHelp :: Comando -> String
getHelp c 
    | null lista = "The \"" ++ c ++ "\"command does not exist." 
    | otherwise = head lista
    where
     lista = [ayuda | (com,ayuda) <- ayudas, c==com]

ayudas :: [(Comando,String)]
ayudas = [("c", "Use: c [-d|-h|-i] [node1 node2..nodeN] : <goal> [; <network>]."
                ++
                "\n\nDescription:\nStandard query of a goal. If no nodes are specified, it is assumed that all are wanted."
                ++
                "If nodes are specified, only the temporal constraints between them will be written. By default, infinite constraints are omitted."
                ++
                "\n\nIf the network is omitted in the goal section, the universal network will be used by default. That is, an empty network."
                ++
                "\n\nOptions:\n-d: Defuzzified constraints.\n-h: Hides the network from the answer.\n-i: Shows the infinite constraints."),
         ("help", "Use: help <comand1> [<comand2>...<comandN>]."
                   ++
                   "\n\nDescription:\nDescribes the usage of the commands passed as arguments."),
         ("load", "Use: load <program_file>."
                   ++
                   "\n\nDescription:\nLoads a FTCProlog program from a file."),
         ("n", "Use: n [-d|-h|-i] [node1 node2..nodeN]."
                ++
                "\n\nDescription:\nShows the next result obtained in the last query."
                ++
                "\n\nOptions:\n-d: Defuzzified constraints.\n-h: Hides the network from the answer.\n-i: Shows the infinite constraints."),
         ("hypo", "Use: hypo <node1> <constraint> <node2>."
                   ++
                   "\n\nDescription:\nCompares the hypothetical constraint between 2 nodes and their real constraint,"
                   ++" using the degree of possibility and necessity for this purpose."
                   ++
                   "\n\nArguments:\n<node1>: initial node of the constraint."
                   ++"\n<relation>: constraint between the nodes, written in a fuzzy temporal language similar to natural language.."
                   ++"\n<node2>: final node of the constraint."),
         ("q","Use: q."
                ++
               "\n\nDescription:\nTerminates the execution of the interpreter."),
         ("last", "Use: last [-d|-h|-i] [node1 node2 .. nodeN]."
                    ++
                   "\n\nDescription:\nReturns the current result."
                    ++
                   "\n\nOptions:\n-d: Defuzzified constraints.\n-h: Hides the network from the answer.\n-i: Shows the infinite constraints."),
         ("resolv", "Uso: resolv [-d] <origin_node> <time> [<node1>...<nodeN>]."
                     ++
                     "\n\nDescription:\nReturns the times of all indicated nodes in the network of the current answer"
                     ++" given a origin node and a time assignment to that node, ignoring the value of the origin node"
                     ++"  in the network, if any. If no nodes are indicated, the times of all nodes in the network are returned."
                     ++"\nThe default answer comes without defuzzification."
                     ++
                     "\n\nOpciones:\n-d: Defuzzifies the result."
                     ++
                     "\n\nArguments:\n<origin_node>: node from which the network is resolved."
                     ++"\n<time>: Time assignment for <origin_node>, in ISO-8601: YYYY-MM-DDTHH:MM:SS format."),
         ("time", "Use: time [-d] [<node1>...<nodeN>]."
                   ++
                   "\n\nDescription: \nReturns the times of the specified nodes of the current answer network,"
                   ++" using the default origin node of the network as the starting point."
                   ++" If no node is indicated, the times of all nodes in the network are returned."
                   ++"\nThe default response is provided without defuzzification."
                   ++
                   "\n\nOptions:\n.-d: Defuzzifies the result."),
         ("succ", "Use: succ <node>."
                    ++
                   "\n\nDescription:\nReturns the nodes that occurred approximately after <node> in the result network."),
         ("pred", "Uso: pred <node>."
                    ++
                   "\n\nDescription:\nReturns the nodes that occurred approximately before <node> in the result network."),
         ("firsts", "Use: firsts."
                     ++
                     "\n\nDescription:\nReturns the nodes representing the initial events of the result network. "
                     ++" Returns more than one if they occurred approximately at the same time."),
         ("lasts", "Use: lasts."
                    ++
                    "\n\nDescription:\nReturns the nodes representing the final events of the result network."
                    ++" Returns more than one if they occurred approximately at the same time.")]

{- Carga un programa a partir de un fichero 
      El primer argumento será el nombre del fichero.
      Si no hay nombre para el fichero no hace nada. -}
load :: ConsultaImp
load es ps [] = 
    do 
        fail "File required.\nUse: load <program_file>."
        return es
load es ps (a:args) = 
    do
        contenido <- readFile a
        let programa = PP.parse contenido
        loadAux es a programa >>= return 

loadAux :: Estado -> String -> Ex ProgramaAmp -> IO Estado
loadAux es file (Ok programa) = 
    do 
        putStrLn ("Program \"" ++ file ++ "\" loaded successfully.")
        return (E programa [])
loadAux es file (Failed err) =
    do
        putStrLn err
        return es

{- Muestra la última respuesta
        Opcion -d: Restricciones defuzzificadas 
        Opcion -h: No muestra la red resultado
        Argumentos: nombres de nodos-}
lastResult :: ConsultaImp
lastResult (E p rs) ps args 
    | null rs = 
        do 
            putStrLn "No answers."
            return (E p rs) 
    | otherwise =
        do 
            let opts = getOpciones ps
            if null args then
                escribeRespuestaCompleta (head rs) (('h' `notElem` opts),('d' `elem` opts),('i' `elem` opts))
            else
                do
                    nodos <- procesaNodos args
                    escribeRespuestaParcial (head rs) nodos (('h' `notElem` opts),('d' `elem` opts),('i' `elem` opts))
            return (E p rs)

{- Muestra la siguiente respuesta
       Opcion -d: Restricciones defuzzificadas
       Opcion -h: No muestra la red resultado
       Argumentos: nombres de nodos 
       TODO: Añadir opciones y argumentos -}
next :: ConsultaImp 
next (E p rs) ps args
    | null rs || null siguientes = 
        do 
            putStrLn "There are no more answers."
            return (E p rs) 
    | otherwise =
        do 
            let opts = getOpciones ps
            let siguientes = tail rs 
            if null args then 
                escribeRespuestaCompleta (head siguientes) (('h' `notElem` opts),('d' `elem` opts),('i' `elem` opts))
            else
                do
                    nodos <- procesaNodos args
                    escribeRespuestaParcial (head siguientes) nodos (('h' `notElem` opts),('d' `elem` opts),('i' `elem` opts))
            return (E p siguientes)
    where
        siguientes = tail rs

{- Muestra la solución de la última red obtenida 
        Opción -d: Solución defuzzificada
        Primer argumento: nodo origen
        Segundo argumento: tiempo asignado a nodo origen
        Tercer argumento: unidades -}
resolv :: ConsultaImp
resolv (E p rs) ps args
    | null rs = 
        do 
            putStrLn "There are no more answers."
            return (E p rs) 
    | length args < 2 = fail "Use: resolv [-d] <origin_node> <time> [<node1>...<nodeN>]."
    | otherwise =
        do
            let opts = getOpciones ps

            nodo <- procesaNodo (args !! 0)
            let tiempo_str = (args !! 1)
            tiempo <- creaTiempoIO tiempo_str
            nodos <- procesaNodos (tail (tail args))
            escribeSolucionRedCustom (head rs) ('d' `elem` opts) nodo tiempo nodos
            return (E p rs)

{- Pregunta por el tiempo de uno o más nodos, 
   usando el nodo origen de la última red obtenida
        Opción -d: Tiempo defuzzificado -}
time :: ConsultaImp
time (E p rs) ps args
    | null rs =
        do 
            putStrLn "There are no more answers."
            return (E p rs) 
    | length args < 0 = fail "Use: time [-d] [<node1>...<nodeN>]."
    | otherwise =
        do
            let opts = getOpciones ps
            nodos <- procesaNodos args
            escribeSolucionRed (head rs) ('d' `elem` opts) nodos
            return (E p rs) 


{- Muestra los eventos sucedidos tras el indicado
        Único argumento: nodo -}
successor :: ConsultaImp
successor (E p rs) ps args
    | null rs =
        do
            putStrLn "There are no more answers."
            return (E p rs)
    | length args /= 1 = fail "Use: succ <node>."
    | otherwise =
        do
            nodo <- procesaNodo (head args)
            let sucesores = getSucesores (head rs) nodo
            putStrLn (concat [(show n)++" " | n <- sucesores])
            return (E p rs)

{- Muestra los eventos sucedidos antes del indicado
        Único argumento: nodo -}
predecessor :: ConsultaImp
predecessor (E p rs) ps args
    | null rs =
        do
            putStrLn "There are no more answers."
            return (E p rs)
    | length args /= 1 = fail "Use: pred <node>."
    | otherwise =
        do
            nodo <- procesaNodo (head args)
            let predecesores = getPredecesores (head rs) nodo
            putStrLn (concat [(show n)++" " | n <- predecesores])
            return (E p rs)

{- Muestra el primer o primeros eventos (si varios ocurrieron aproximadamente
   al mismo tiempo) de la red de la respuesta -}
firsts :: ConsultaImp
firsts (E p rs) ps args 
    | null rs =
        do 
            putStrLn "There are no more answers."
            return (E p rs)
    | length args /= 0 = fail "Use: firsts."
    | otherwise =
        do
            let primeros = getPrimeros (head rs)
            putStrLn (concat [(show n)++" " | n <- primeros])
            return (E p rs)

{- Muestra el último o últimos eventos (si varios ocurrieron aproximadamente
   al mismo tiempo) de la red de la respuesta -}
lasts :: ConsultaImp
lasts (E p rs) ps args 
    | null rs =
        do 
            putStrLn "There are no more answers."
            return (E p rs)
    | length args /= 0 = fail "Use: lasts."
    | otherwise =
        do
            let ultimos = getFinales (head rs)
            putStrLn (concat [(show n)++" " | n <- ultimos])
            return (E p rs)

{- Compara una relación hipotética entre 2 nodos con la restricción equivalente de la red -}
hypoComp :: ConsultaImp
hypoComp (E p rs) ps args
    | null rs =
        do
            putStrLn "There are no more answers."
            return (E p rs)
    | length args /= 3 = fail "Use: hypo <node1> <relation> <node2>."
    | otherwise =
        do
            nodo1 <- procesaNodo (args !! 0)
            let rel = HP.parse (args !! 1)
            nodo2 <- procesaNodo (args !! 2)

            hypoCompAux (E p rs) nodo1 nodo2 rel >>= return

hypoCompAux :: Estado -> Nodo -> Nodo -> Ex Relacion -> IO Estado
hypoCompAux (E p rs) n1 n2 (Ok rel) =
        do
            let restr = getRestriccion (head rs) n1 n2
            let hypo = escala (getDist rel) (segundos (getUT rel))

            let pos = posibilidad restr hypo
            let nec = necesidad restr hypo 

            putStrLn ("Degree of possibility: " ++ show pos)
            putStrLn ("Degree of necessity: " ++ show nec)

            return (E p rs)
hypoCompAux es _ _ (Failed err) =
        do
            putStrLn err
            return es

{- Pone el intérprete en su estado final -}
quit :: ConsultaImp
quit es ps args =
    do
        putStrLn "Exiting..."
        return EFinal

----------------------------COMANDOS CON OBJETIVO-------------------------------------
{- Comando vacío -}
noCommandObjective :: Comando -> ConsultaObjetivoImp
noCommandObjective com es _ _ _ = 
    do 
        fail ("The \"" ++ com ++ "\"command does not exist.")
        return es

{- Consulta normal de un objetivo
      Opcion -d: Restricciones defuzzificadas
      Opcion -h: Oculta la red de la respuesta
      TODO: Añadir opciones y argumentos -}
consult :: ConsultaObjetivoImp
consult (E p rs) ps args obj =
    do
        let opts = getOpciones ps
        let respuestas = respuestasReducidasAmp p obj
        if null respuestas then
            do
                putStrLn "No answers."
        else
            do 
                if null args then
                    escribeRespuestaCompleta (head respuestas) (('h' `notElem` opts),('d' `elem` opts),('i' `elem` opts))
                else 
                    do
                        nodos <- procesaNodos args
                        escribeRespuestaParcial (head respuestas) nodos (('h' `notElem` opts),('d' `elem` opts),('i' `elem` opts))
        return (E p respuestas)

-----------------------------AUXILIAR-----------------------------
procesaNodo :: String -> IO Nodo
procesaNodo str = 
    do
        let sep = splitOn "=" str
        case (length sep) of
            1 -> return (creaNodo (head sep))
            2 -> return (creaNodoVal (head sep) (last sep))
            otherwise -> fail (str ++ "  is not a node.")

procesaNodos :: [String] -> IO [Nodo]
procesaNodos [] = do return []
procesaNodos (s:strs) =
    do
        nodo <- procesaNodo s
        resto <- procesaNodos strs
        return (nodo:resto)