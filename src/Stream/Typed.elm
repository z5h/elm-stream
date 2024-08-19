module Stream.Typed exposing (Finite, MaybeInfinite, Stream, append, apply, concatMap, constant, dropN, dropWhile, empty, equals, filter, filterMap, flatten, fold, fromArray, fromArrayAt, fromGenerator, fromList, head, isEmpty, keepIf, last, lazy, length, loop, map, member, product, range, rangeFrom, singleton, takeN, takeWhile, toList, zip, zipFiniteL, zipFiniteR)

import Array exposing (Array)
import Stream


type Stream length a
    = Stream (Stream.Stream a)


type Finite
    = Finite


type MaybeInfinite
    = MaybeInfinite


empty : Stream finite a
empty =
    Stream Stream.empty


singleton : a -> Stream finite a
singleton x =
    Stream (Stream.singleton x)


constant : a -> Stream finite a
constant a =
    Stream (Stream.constant a)


lazy : (() -> Stream length a) -> Stream length a
lazy f =
    Stream
        (Stream.lazy
            (\_ ->
                f () |> toStream
            )
        )


range : Int -> Int -> Stream finite Int
range from to =
    Stream (Stream.range from to)


rangeFrom : Int -> Stream finite Int
rangeFrom from =
    Stream (Stream.rangeFrom from)


fromGenerator : (a -> Maybe a) -> a -> Stream MaybeInfinite a
fromGenerator generator start =
    Stream (Stream.fromGenerator generator start)


fromList : List a -> Stream finite a
fromList list =
    Stream (Stream.fromList list)


fromArray : Array a -> Stream finite a
fromArray a =
    Stream (Stream.fromArray a)


fromArrayAt : Int -> Array a -> Stream finite a
fromArrayAt i a =
    Stream (Stream.fromArrayAt i a)


append : Stream length a -> Stream length a -> Stream length a
append (Stream l) (Stream r) =
    Stream (Stream.append l r)


product : Stream length a -> Stream length b -> Stream length ( a, b )
product (Stream l) (Stream r) =
    Stream (Stream.product l r)


zip : Stream length a -> Stream length b -> Stream length ( a, b )
zip (Stream l) (Stream r) =
    Stream (Stream.zip l r)


zipFiniteL : Stream Finite a -> Stream length b -> Stream finite ( a, b )
zipFiniteL (Stream l) (Stream r) =
    Stream (Stream.zip l r)


zipFiniteR : Stream length a -> Stream Finite b -> Stream finite ( a, b )
zipFiniteR (Stream l) (Stream r) =
    Stream (Stream.zip l r)


apply : Stream length a -> Stream length (a -> b) -> Stream length b
apply xs fs =
    product xs fs |> map (\( x, f ) -> f x)


map : (a -> b) -> Stream length a -> Stream length b
map f (Stream xs) =
    Stream (Stream.map f xs)


concatMap : (a -> Stream length b) -> Stream length a -> Stream length b
concatMap f (Stream xs) =
    Stream
        (Stream.concatMap (\x -> toStream (f x)) xs)


fold : (a -> acc -> acc) -> acc -> Stream length a -> Stream length acc
fold f acc (Stream s) =
    Stream (Stream.fold f acc s)


filter : (a -> Bool) -> Stream length a -> Stream length a
filter f (Stream s) =
    Stream (Stream.filter f s)


keepIf : (a -> Bool) -> Stream length a -> Stream length a
keepIf f (Stream s) =
    Stream (Stream.keepIf f s)


filterMap : (a -> Maybe b) -> Stream length a -> Stream length b
filterMap f (Stream s) =
    Stream (Stream.filterMap f s)


loop : Stream length a -> Stream MaybeInfinite a
loop (Stream s) =
    Stream (Stream.loop s)


flatten : Stream length (Stream length a) -> Stream length a
flatten (Stream s) =
    Stream (Stream.flatten (Stream.map toStream s))


dropN : Int -> Stream length a -> Stream length a
dropN i (Stream s) =
    Stream (Stream.dropN i s)


dropWhile : (a -> Bool) -> Stream length a -> Stream length a
dropWhile f (Stream s) =
    Stream (Stream.dropWhile f s)


takeN : Int -> Stream length a -> Stream finite a
takeN n (Stream s) =
    Stream (Stream.takeN n s)


takeWhile : (a -> Bool) -> Stream length a -> Stream length a
takeWhile f (Stream s) =
    Stream (Stream.takeWhile f s)


isEmpty : Stream length a -> Bool
isEmpty (Stream f) =
    Stream.isEmpty f


equals : Stream Finite a -> Stream Finite a -> Bool
equals (Stream l) (Stream r) =
    Stream.equals l r |> Stream.force


member : a -> Stream Finite a -> Bool
member elem (Stream s) =
    Stream.member elem s |> Stream.force


length : Stream Finite a -> Int
length (Stream s) =
    Stream.length s |> Stream.force


toList : Stream Finite a -> List a
toList (Stream s) =
    Stream.toList s |> Stream.force


head : Stream length a -> Maybe a
head (Stream s) =
    Stream.head s


last : Stream Finite a -> Maybe a
last (Stream s) =
    s |> Stream.last |> Stream.force


toStream : Stream length b -> Stream.Stream b
toStream (Stream s) =
    s
