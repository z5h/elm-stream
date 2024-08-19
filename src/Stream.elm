module Stream exposing (Outcome(..), Stream, append, apply, concatMap, constant, dropN, dropWhile, empty, equals, filter, filterMap, flatten, fold, force, fromArray, fromArrayAt, fromGenerator, fromList, head, isEmpty, isFinite, isInfinite, keepIf, last, lazy, length, loop, map, member, product, range, rangeFrom, singleton, takeN, takeWhile, toList, zip)

import Array exposing (Array)
import Stream.Internal as Internal
import Stream.Raw as Raw


type Stream a
    = Stream Length (Internal.Stream a)


type Length
    = Finite
    | Infinite
    | Indefinite


type Outcome a
    = Known a
    | Unsafe (() -> a)


empty : Stream a
empty =
    Stream Finite Raw.empty


singleton : a -> Stream a
singleton x =
    Stream Finite (Raw.singleton x)


constant : a -> Stream a
constant a =
    Stream Infinite (Raw.constant a)


lazy : (() -> Stream a) -> Stream a
lazy f =
    Stream Indefinite
        (Raw.lazy
            (\_ ->
                f () |> toRaw
            )
        )


range : Int -> Int -> Stream Int
range from to =
    Stream Finite (Raw.range from to)


rangeFrom : Int -> Stream Int
rangeFrom from =
    Stream Infinite (Raw.rangeFrom from)


fromGenerator : (a -> Maybe a) -> a -> Stream a
fromGenerator generator start =
    Stream Indefinite (Raw.fromGenerator generator start)


fromList : List a -> Stream a
fromList list =
    Stream Finite (Raw.fromList list)


fromArray : Array a -> Stream a
fromArray a =
    Stream Finite (Raw.fromArray a)


fromArrayAt : Int -> Array a -> Stream a
fromArrayAt i a =
    Stream Finite (Raw.fromArrayAt i a)


append : Stream a -> Stream a -> Stream a
append (Stream ll l) (Stream rl r) =
    Stream
        (if ll == Infinite || rl == Infinite then
            Infinite

         else if ll == Finite && rl == Finite then
            Finite

         else
            Indefinite
        )
        (Raw.append l r)


product : Stream a -> Stream b -> Stream ( a, b )
product (Stream ll l) (Stream rl r) =
    Stream
        (if ll == Infinite || rl == Infinite then
            Infinite

         else if ll == Finite && rl == Finite then
            Finite

         else
            Indefinite
        )
        (Raw.product l r)


zip : Stream a -> Stream b -> Stream ( a, b )
zip (Stream ll l) (Stream rl r) =
    Stream
        (if ll == Infinite && rl == Infinite then
            Infinite

         else if ll == Finite || rl == Finite then
            Finite

         else
            Indefinite
        )
        (Raw.zip l r)


apply : Stream a -> Stream (a -> b) -> Stream b
apply xs fs =
    product xs fs |> map (\( x, f ) -> f x)


map : (a -> b) -> Stream a -> Stream b
map f (Stream l xs) =
    Stream l (Raw.map f xs)


concatMap : (a -> Stream b) -> Stream a -> Stream b
concatMap f (Stream l xs) =
    Stream
        (if l == Infinite then
            Infinite

         else
            Indefinite
        )
        (Raw.concatMap (\x -> toRaw (f x)) xs)


fold : (a -> acc -> acc) -> acc -> Stream a -> Stream acc
fold f acc (Stream l s) =
    Stream l (Raw.fold f acc s)


filter : (a -> Bool) -> Stream a -> Stream a
filter f (Stream l s) =
    Stream l (Raw.filter f s)


keepIf : (a -> Bool) -> Stream a -> Stream a
keepIf f (Stream l s) =
    Stream l (Raw.keepIf f s)


filterMap : (a -> Maybe b) -> Stream a -> Stream b
filterMap f (Stream l s) =
    Stream l (Raw.filterMap f s)


loop : Stream a -> Stream a
loop (Stream _ s) =
    Stream Infinite (Raw.loop s)


flatten : Stream (Stream a) -> Stream a
flatten (Stream l s) =
    Stream
        (if l == Infinite then
            Infinite

         else
            Indefinite
        )
        (Raw.flatten (Raw.map toRaw s))


dropN : Int -> Stream a -> Stream a
dropN i (Stream len s) =
    Stream len (Raw.dropN i s)


dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile f (Stream len s) =
    Stream len (Raw.dropWhile f s)


takeN : Int -> Stream a -> Stream a
takeN n (Stream _ s) =
    Stream Finite (Raw.takeN n s)


takeWhile : (a -> Bool) -> Stream a -> Stream a
takeWhile f (Stream len s) =
    Stream
        (if len == Finite then
            Finite

         else
            Indefinite
        )
        (Raw.takeWhile f s)


isEmpty : Stream a -> Bool
isEmpty (Stream _ f) =
    Raw.isEmpty f


equals : Stream a -> Stream a -> Outcome Bool
equals (Stream ll l) (Stream rl r) =
    if ll == Finite && rl == Finite then
        Known (Raw.equals l r)

    else
        Unsafe (\_ -> Raw.equals l r)


member : a -> Stream a -> Outcome Bool
member elem (Stream l s) =
    if l == Finite then
        Known (Raw.member elem s)

    else
        Unsafe (\_ -> Raw.member elem s)


length : Stream a -> Outcome Int
length (Stream l s) =
    if l == Finite then
        Known (Raw.length s)

    else
        Unsafe (\_ -> Raw.length s)


isFinite : Stream a -> Outcome Bool
isFinite (Stream l s) =
    case l of
        Finite ->
            Known True

        Infinite ->
            Known False

        Indefinite ->
            -- `length` is a bogus function to force the stream
            Unsafe (\_ -> Raw.length s >= 0)


isInfinite : Stream a -> Outcome Bool
isInfinite (Stream l s) =
    case l of
        Finite ->
            Known False

        Infinite ->
            Known True

        Indefinite ->
            -- `length` is a bogus function to force the stream
            Unsafe (\_ -> Raw.length s < 0)


toList : Stream a -> Outcome (List a)
toList (Stream l s) =
    if l == Finite then
        Known (Raw.toList s)

    else
        Unsafe (\_ -> Raw.toList s)


head : Stream a -> Maybe a
head (Stream _ s) =
    Raw.head s


last : Stream a -> Outcome (Maybe a)
last (Stream l s) =
    if l == Finite then
        Known (Raw.last s)

    else
        Unsafe (\_ -> Raw.last s)


toRaw : Stream b -> Raw.Stream b
toRaw (Stream _ s) =
    s


force : Outcome a -> a
force safety =
    case safety of
        Known a ->
            a

        Unsafe thunk ->
            thunk ()
