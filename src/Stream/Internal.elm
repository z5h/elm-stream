module Stream.Internal exposing (Stream(..))


type Stream a
    = Empty
    | Mature a (Stream a)
    | Immature (() -> Stream a)
