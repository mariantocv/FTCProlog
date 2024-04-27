module Exception where

data Ex a = Ok a | Failed String

thenE :: Ex a -> (a -> Ex b) -> Ex b
m `thenE` k =
    case m of
        Ok a -> k a
        Failed e -> Failed e

returnE :: a -> Ex a
returnE a = Ok a

failE :: String -> Ex a
failE err = Failed err

catchE :: Ex a -> (String -> Ex a) -> Ex a
catchE m k =
    case m of
        Ok a -> Ok a
        Failed e -> k e