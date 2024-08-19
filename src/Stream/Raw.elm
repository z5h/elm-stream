module Stream.Raw exposing
    ( Stream
    , empty, singleton, constant, lazy, range, rangeFrom, fromGenerator
    , fromList, fromArray, fromArrayAt
    , append, product, zip, apply
    , map, concatMap, fold, filter, keepIf, filterMap, takeN, dropN, dropWhile, takeWhile, flatten, loop
    , length, equals, member
    , isEmpty, head, last, toList
    )

{-| A `Stream` is a lazy sequence. They are generated/mapped/transformed (as needed).
It may be infinite, and infinite streams are ok to pass as arguments. As long as you don't ask for
the last element, (or length) you should be fine.

The design of this module was inspired by Kanren's `Stream` type, and was motivated by limitations
in existing lazy sequence libraries in Elm.

The most general way to create a (possibly infinite) custom stream is something like:

    myInitialState
        |> fromGenerator
            (\myState ->
                if myKeepGoing myState then
                    Just (myNextState myState)

                else
                    Nothing
            )
        |> map (\myGeneratorState -> myGetGeneratedValue myGeneratedState)


# Definition

@docs Stream


# Creation

@docs empty, singleton, constant, lazy, range, rangeFrom, fromGenerator


# Create from other data structures

@docs fromList, fromArray, fromArrayAt


# Combine

@docs append, product, zip, apply


# Transform

@docs map, concatMap, fold, filter, keepIf, filterMap, takeN, dropN, dropWhile, takeWhile, flatten, loop


# Queries

@docs length, equals, member


# Deconstruct

@docs isEmpty, head, last, toList

-}

import Array exposing (Array)
import Stream.Internal as Internal exposing (Stream(..))



-- Definition


{-| Internally, a stream is either:

  - `Empty` (no more elements)
  - `Mature a (Stream a)` (a value and more to come)
  - `Immature (() -> Stream a)` (a function that will return a stream if/when called)

-}
type alias Stream a =
    Internal.Stream a



{- Creation -}


{-| An empty stream.
-}
empty : Stream a
empty =
    Empty


{-| A stream with a single element. The stream will be empty after this element.
-}
singleton : a -> Stream a
singleton a =
    Mature a Empty


{-| A stream that repeats the same input value forever.
-}
constant : a -> Stream a
constant v =
    loop (singleton v)


{-| Create a lazy stream from any stream-generating function.
-}
lazy : (() -> Stream a) -> Stream a
lazy thunk =
    Immature thunk


{-| A stream containing all the integers from `start` to `end` (inclusive).
-}
range : Int -> Int -> Stream Int
range start end =
    if start > end then
        Empty

    else if start == end then
        singleton start

    else
        start
            |> fromGenerator
                (\n ->
                    if n < end then
                        Just (n + 1)

                    else
                        Nothing
                )


{-| An infinite stream containing all the integers from `n` to infinity.
-}
rangeFrom : Int -> Stream Int
rangeFrom from =
    fromGenerator (\n_ -> Just (n_ + 1)) from


{-| Use a generator function to create a stream.
-}
fromGenerator : (a -> Maybe a) -> a -> Stream a
fromGenerator f a =
    Mature a
        (Immature
            (\() ->
                case f a of
                    Just next ->
                        fromGenerator f next

                    Nothing ->
                        Empty
            )
        )



{- Create from other data structures -}


{-| Convert a list to a stream.
-}
fromList : List a -> Stream a
fromList l =
    case l of
        [] ->
            Empty

        first :: rest ->
            Mature first (Immature (\() -> fromList rest))


{-| Convert an array to a stream.
-}
fromArray : Array a -> Stream a
fromArray a =
    fromArrayAt 0 a


{-| Convert an array to a stream, starting at the given index.
More efficient than `fromArray |> dropN i` if you intend to skip
the first `i` elements.

Negative and out-of-bounds indices will result in an empty stream.

-}
fromArrayAt : Int -> Array a -> Stream a
fromArrayAt i a =
    if i < 0 then
        Empty

    else
        case Array.get i a of
            Just v ->
                Mature v (Immature (\() -> fromArrayAt (i + 1) a))

            Nothing ->
                Empty



{- Combine -}


{-| A stream who's contents are all of the contents of the first (possibly infinite) stream followed by the second stream.
-}
append : Stream a -> Stream a -> Stream a
append left right =
    case ( left, right ) of
        ( Empty, _ ) ->
            right

        ( _, Empty ) ->
            left

        ( Mature a Empty, _ ) ->
            Mature a right

        ( Mature a followingStream, _ ) ->
            Mature a (Immature (\() -> append followingStream right))

        ( Immature lazyStream, _ ) ->
            Immature (\() -> append (lazyStream ()) right)


{-| Generates all possible tuples from two streams.
Streams may be infinite, and `product` ensures that any particular
tuple will be processed in a finite number of steps. There is no ordering guarantee on the output.
-}
product : Stream a -> Stream b -> Stream ( a, b )
product sa sb =
    sa
        |> map (\a -> map (\b -> ( a, b )) sb)
        |> flatten


{-| Zip two streams together. End when the shorter of the two ends.
-}
zip : Stream a -> Stream b -> Stream ( a, b )
zip sa sb =
    case ( sa, sb ) of
        ( Empty, _ ) ->
            Empty

        ( _, Empty ) ->
            Empty

        ( Mature a sa_, Mature b sb_ ) ->
            Mature ( a, b ) (Immature (\() -> zip sa_ sb_))

        ( Immature thunka, _ ) ->
            Immature (\() -> zip (thunka ()) sb)

        ( _, Immature thunkb ) ->
            Immature (\() -> zip sa (thunkb ()))


{-| Apply a stream of functions to a stream of values.
Useful for building a stream of all possible values out of
a (singleton) constructor and streams of parameters.

    Stream.singleton Triple
        |> Stream.apply firstValueStream
        |> Stream.apply secondValueStream
        |> Stream.apply thirdValueStream

-}
apply : Stream a -> Stream (a -> b) -> Stream b
apply xs fs =
    product xs fs |> map (\( x, f ) -> f x)


applyInlined : Stream a -> Stream (a -> b) -> Stream b
applyInlined xs fs =
    xs
        |> map (\x -> map (\f -> f x) fs)
        |> flatten



{- Transform -}


{-| Apply a function to each element of the stream and return a new stream of the results.
-}
map : (a -> b) -> Stream a -> Stream b
map f s =
    case s of
        Empty ->
            Empty

        Mature a sa ->
            Mature (f a) (Immature (\() -> map f sa))

        Immature thunk ->
            Immature (\() -> map f (thunk ()))


{-| Apply a stream-generating function to each element of the stream and return a new stream of all of
the results.
-}
concatMap : (a -> Stream b) -> Stream a -> Stream b
concatMap f s =
    case s of
        Empty ->
            Empty

        Mature a sa ->
            append (f a) (Immature (\() -> concatMap f sa))

        Immature thunk ->
            Immature (\() -> concatMap f (thunk ()))


concatMap2 f s =
    map f s |> flatten


{-| Reduce a stream with an accumulator function. And produce a new stream of the accumulated values.
If a stream is finite, `last |> Maybe.withDefault acc` will be the final accumulated value.
-}
fold : (a -> acc -> acc) -> acc -> Stream a -> Stream acc
fold f acc s =
    case s of
        Empty ->
            Empty

        Mature a rest ->
            let
                nextAcc =
                    f a acc
            in
            Mature nextAcc (Immature (\() -> fold f nextAcc rest))

        Immature thunk ->
            Immature (\() -> fold f acc (thunk ()))


{-| Filter the stream based on a predicate function.
Aliased as `keepIf`.
-}
filter : (a -> Bool) -> Stream a -> Stream a
filter f =
    filterMap
        (\a ->
            if f a then
                Just a

            else
                Nothing
        )


{-| Alias for `filter`.
-}
keepIf : (a -> Bool) -> Stream a -> Stream a
keepIf =
    filter


{-| Filter the stream based on a mapping function.
-}
filterMap : (a -> Maybe b) -> Stream a -> Stream b
filterMap f s =
    case s of
        Empty ->
            Empty

        Mature a sa ->
            case f a of
                Just b ->
                    Mature b (Immature (\() -> filterMap f sa))

                Nothing ->
                    filterMap f sa

        Immature thunk ->
            Immature (\() -> filterMap f (thunk ()))


{-| A stream that repeats the same input stream value forever.
-}
loop : Stream a -> Stream a
loop s =
    case s of
        Empty ->
            Empty

        _ ->
            append s (Immature (\() -> loop s))


{-| Flatten a (possibly infinite) stream of (possibly infine) streams into a single stream, in
such a way that we guarantee any indexable element will be processed in a finite number of steps.

For example:

    rangeFrom 0 |> map (\\int -> rangeFrom 0 |> map (\\i -> ( i, int ))) |> omega

will produce a stream of tuples such that `filter ((==) (m,n)) |> head` will return `Just (m,n)`
in a finite number of steps.

-}
flatten : Stream (Stream a) -> Stream a
flatten l =
    flattenHelp_ (Flatten_ { untouched = l, touched = [], waiting = [] })


type Flatten_ a
    = Flatten_ { untouched : Stream (Stream a), touched : List (Stream a), waiting : List (Stream a) }


flattenHelp_ : Flatten_ a -> Stream a
flattenHelp_ (Flatten_ { untouched, touched, waiting }) =
    case ( untouched, touched, waiting ) of
        -- nothing to do
        ( Empty, [], [] ) ->
            Empty

        -- defer
        ( Immature thunk, _, _ ) ->
            Immature (\() -> flattenHelp_ (Flatten_ { untouched = thunk (), touched = touched, waiting = waiting }))

        -- actual work to do, waiting is empty, touched is empty
        ( Mature u us, [], [] ) ->
            headThen u
                (\u_ -> flattenHelp_ (Flatten_ { untouched = us, touched = [ u_ ], waiting = [] }))
                (\() -> flattenHelp_ (Flatten_ { untouched = us, touched = [], waiting = [] }))

        -- work to do, waiting is not empty, touched is fully processed
        ( Mature u us, [], _ ) ->
            headThen u
                (\u_ -> flattenHelp_ (Flatten_ { untouched = us, touched = u_ :: waiting, waiting = [] }))
                (\() -> flattenHelp_ (Flatten_ { untouched = us, touched = waiting, waiting = [] }))

        -- touched to be processed
        ( _, t :: ts, _ ) ->
            headThen t
                (\t_ -> flattenHelp_ (Flatten_ { untouched = untouched, touched = ts, waiting = t_ :: waiting }))
                (\() -> flattenHelp_ (Flatten_ { untouched = untouched, touched = ts, waiting = waiting }))

        ( Empty, [], _ ) ->
            flattenHelp_ (Flatten_ { untouched = Immature (\() -> Empty), touched = waiting, waiting = [] })


{-| Drop up to `n` values from the stream, so long as there are values to drop.
Dropping any number of values from an empty stream results in an empty stream.
-}
dropN : Int -> Stream a -> Stream a
dropN n s =
    if n <= 0 then
        s

    else
        case s of
            Empty ->
                Empty

            Mature _ rest ->
                dropN (n - 1) rest

            Immature thunk ->
                Immature (\() -> dropN n (thunk ()))


dropWhile : (a -> Bool) -> Stream a -> Stream a
dropWhile f s =
    case s of
        Empty ->
            Empty

        Mature a rest ->
            if f a then
                Immature (\() -> dropWhile f rest)

            else
                Mature a rest

        Immature thunk ->
            Immature (\() -> dropWhile f (thunk ()))


{-| Take up to `n` values from the stream, so long as there are values to take.
The returned stream is a stream of these values.
-}
takeN : Int -> Stream a -> Stream a
takeN n s =
    if n <= 0 then
        Empty

    else
        case s of
            Empty ->
                Empty

            Immature th ->
                Immature (\() -> takeN n (th ()))

            Mature a stream ->
                Mature a (takeN (n - 1) stream)


takeWhile : (a -> Bool) -> Stream a -> Stream a
takeWhile f s =
    case s of
        Empty ->
            Empty

        Mature a rest ->
            if f a then
                Mature a (Immature (\() -> takeWhile f rest))

            else
                Empty

        Immature thunk ->
            Immature (\() -> takeWhile f (thunk ()))



{- Queries -}


{-| Tests if the stream contains no elements.
-}
isEmpty : Stream a -> Bool
isEmpty s =
    case s of
        Empty ->
            True

        Immature thunk ->
            isEmpty (thunk ())

        _ ->
            False


{-| Tests if two streams are equal in terms of content and order of the content.
Will not return for infinite streams that are equal.
-}
equals : Stream a -> Stream a -> Bool
equals s1 s2 =
    case ( s1, s2 ) of
        ( Empty, Empty ) ->
            True

        ( Immature thunk, _ ) ->
            equals (thunk ()) s2

        ( _, Immature thunk ) ->
            equals s1 (thunk ())

        ( Mature a1 s1_, Mature a2 s2_ ) ->
            a1 == a2 && equals s1_ s2_

        _ ->
            False


{-| Tests if element is a member of the stream.
Will not return from an infinite stream if the element is not found.
-}
member : a -> Stream a -> Bool
member a s =
    case s of
        Empty ->
            False

        Mature a_ s_ ->
            if a == a_ then
                True

            else
                member a s_

        Immature thunk ->
            member a (thunk ())


{-| Returns the length of the stream (by generating and counting all elements).
Will not return for infinite streams.
-}
length : Stream a -> Int
length s =
    fold (\_ acc -> acc + 1) 0 s |> last |> Maybe.withDefault 0



{- Deconstruct -}


{-| Generate and return the contents of the stream. Will not return for infinite streams.
-}
toList : Stream a -> List a
toList s =
    s |> fold (::) [] |> last |> Maybe.withDefault [] |> List.reverse


{-| Returns the first element of the stream, or `Nothing` if the stream is empty.
-}
head : Stream a -> Maybe a
head s =
    case s of
        Empty ->
            Nothing

        Mature a _ ->
            Just a

        Immature thunk ->
            head (thunk ())


{-| Generates and traverses the stream, returning the last element. Will not return for infinite streams.
-}
last : Stream a -> Maybe a
last s =
    case s of
        Empty ->
            Nothing

        Mature a Empty ->
            Just a

        Mature a (Immature thunk) ->
            last (Mature a (thunk ()))

        Mature _ rest ->
            last rest

        Immature thunk ->
            last (thunk ())


{-| Helper function.
Creates a new stream from the head of the input stream, and then (lazily)
applies the first function to the rest of the input stream.
If the input stream is empty, the second function is applied.
-}
headThen : Stream a -> (Stream a -> Stream a) -> (() -> Stream a) -> Stream a
headThen s whenNotEmpty whenEmpty =
    case s of
        Empty ->
            whenEmpty ()

        Mature a sa ->
            Mature a (Immature (\() -> whenNotEmpty sa))

        Immature thunk ->
            headThen (thunk ()) whenNotEmpty whenEmpty
