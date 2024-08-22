module Stream.Raw exposing
    ( Stream
    , empty, singleton, constant, lazy, range, rangeFrom, fromGenerator
    , fromList, fromArray, fromArrayAt
    , append, product, zip, apply
    , map, fold, filter, keepIf, filterMap, takeN, dropN, dropWhile, loop
    , length, equals, member
    , isEmpty, head, last, toList
    , flattenN
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
type Stream length a
    = Empty length
    | Mature a (Stream length a)
    | Immature (() -> Stream length a)


{-| Indicates the stream's content count is finite.
-}
type Finite
    = Finite


{-| Indicates the stream's content count is unknown, but any content is guaranteed to be reachable in a finite number of steps.
-}
type Infinite
    = Infinite Infinite


impossible : Infinite -> a
impossible (Infinite i) =
    impossible i



{- Creation -}


{-| An empty stream.
-}
empty : Stream Finite a
empty =
    Empty Finite


{-| A stream with a single element. The stream will be empty after this element.
-}
singleton : a -> Stream Finite a
singleton a =
    Mature a empty


{-| A stream that repeats the same input value forever.
-}
constant : a -> Stream Infinite a
constant v =
    loop (singleton v)


{-| Create a lazy stream from any stream-generating function.
-}
lazy : (() -> Stream l a) -> Stream l a
lazy thunk =
    Immature thunk


{-| A stream containing all the integers from `start` to `end` (inclusive).
-}
range : Int -> Int -> Stream Finite Int
range start end =
    if start > end then
        empty

    else if start == end then
        singleton start

    else
        let
            delta =
                if start < end then
                    1

                else
                    -1
        in
        start
            |> fromFiniteGenerator (\v -> abs (end - v))
                (\n ->
                    if n < end then
                        Just (n + delta)

                    else
                        Nothing
                )


{-| An infinite stream containing all the integers from `n` to infinity.
-}
rangeFrom : Int -> Stream Infinite Int
rangeFrom from =
    fromGenerator (\n_ -> n_ + 1) from


{-| Use a generator function to create a stream.
-}
fromGenerator : (a -> a) -> a -> Stream Infinite a
fromGenerator f a =
    Mature
        a
        (Immature
            (\() ->
                fromGenerator f (f a)
            )
        )


type alias Distance a =
    a -> Int


fromFiniteGenerator : Distance a -> (a -> Maybe a) -> a -> Stream Finite a
fromFiniteGenerator fdistance f a =
    Mature a
        (Immature
            (\() ->
                case f a of
                    Just next ->
                        let
                            distance =
                                fdistance a

                            distanceNext =
                                fdistance next
                        in
                        if distanceNext >= distance then
                            empty

                        else if distanceNext > 0 then
                            fromFiniteGenerator fdistance f next

                        else if distanceNext == 0 then
                            singleton next

                        else
                            empty

                    Nothing ->
                        empty
            )
        )



{- Create from other data structures -}


{-| Convert a list to a stream.
-}
fromList : List a -> Stream Finite a
fromList l =
    case l of
        [] ->
            empty

        first :: rest ->
            Mature first (Immature (\() -> fromList rest))


{-| Convert an array to a stream.
-}
fromArray : Array a -> Stream Finite a
fromArray a =
    fromArrayAt 0 a


{-| Convert an array to a stream, starting at the given index.
More efficient than `fromArray |> dropN i` if you intend to skip
the first `i` elements.

Negative and out-of-bounds indices will result in an empty stream.

-}
fromArrayAt : Int -> Array a -> Stream Finite a
fromArrayAt i a =
    if i < 0 then
        empty

    else
        case Array.get i a of
            Just v ->
                Mature v (Immature (\() -> fromArrayAt (i + 1) a))

            Nothing ->
                empty



{- Combine -}


{-| A stream who's contents are all of the contents of the first (possibly infinite) stream followed by the second stream.
-}
append : Stream Finite a -> Stream l a -> Stream l a
append left right =
    case ( left, right ) of
        ( Empty Finite, _ ) ->
            right

        ( Mature a Empty, _ ) ->
            Mature a right

        ( Mature a followingStream, _ ) ->
            Mature a (Immature (\() -> append followingStream right))

        ( Immature lazyStream, _ ) ->
            Immature (\() -> append (lazyStream ()) right)


weave : Stream Infinite a -> Stream Infinite a -> Stream Infinite a
weave s1 s2 =
    case ( s1, s2 ) of
        ( Empty infinite, _ ) ->
            impossible infinite

        ( _, Empty infinite ) ->
            impossible infinite

        ( Mature a s1_, Mature b s2_ ) ->
            Mature a (Mature b (Immature (\() -> weave s1_ s2_)))

        ( Immature thunk1, _ ) ->
            Immature (\() -> weave (thunk1 ()) s2)

        ( _, Immature thunk2 ) ->
            Immature (\() -> weave s1 (thunk2 ()))


{-| Generates all possible tuples from two streams.
Streams may be infinite, and `product` ensures that any particular
tuple will be processed in a finite number of steps. There is no ordering guarantee on the output.
-}
product : Stream l1 a -> Stream l2 b -> Stream l1 (Stream l2 ( a, b ))
product sa sb =
    sa |> map (\a -> map (\b -> ( a, b )) sb)


syncWith : Stream Finite b -> Stream Infinite a -> Stream Finite a
syncWith finiteStream stream =
    case ( finiteStream, stream ) of
        ( Empty _, _ ) ->
            empty

        ( Mature _ finiteRest, Mature a reachableRest ) ->
            Mature a (Immature (\() -> syncWith finiteRest reachableRest))

        ( Immature thunk, _ ) ->
            Immature (\() -> syncWith (thunk ()) stream)

        ( _, Immature thunk ) ->
            syncWith finiteStream (thunk ())


{-| Zip two streams together. End when the shorter of the two ends.
-}
zip : (a -> b -> c) -> Stream l a -> Stream l b -> Stream l c
zip f sa sb =
    case ( sa, sb ) of
        ( Empty e, _ ) ->
            Empty e

        ( _, Empty e ) ->
            Empty e

        ( Mature a sa_, Mature b sb_ ) ->
            Mature (f a b) (Immature (\() -> zip f sa_ sb_))

        ( Immature thunka, _ ) ->
            Immature (\() -> zip f (thunka ()) sb)

        ( _, Immature thunkb ) ->
            Immature (\() -> zip f sa (thunkb ()))


{-| Apply a stream of functions to a stream of values.
Useful for building a stream of all possible values out of
a (singleton) constructor and streams of parameters.

    Stream.singleton Triple
        |> Stream.apply firstValueStream
        |> Stream.apply secondValueStream
        |> Stream.apply thirdValueStream

-}
apply : Stream l1 a -> Stream l2 (a -> b) -> Stream l1 (Stream l2 b)
apply xs fs =
    xs
        |> map (\x -> map (\f -> f x) fs)



{- Transform -}


{-| Apply a function to each element of the stream and return a new stream of the results.
-}
map : (a -> b) -> Stream length a -> Stream length b
map f s =
    case s of
        Empty e ->
            Empty e

        Mature a sa ->
            Mature (f a) (Immature (\() -> map f sa))

        Immature thunk ->
            Immature (\() -> map f (thunk ()))


{-| Reduce a stream with an accumulator function. And produce a new stream of the accumulated values.
If a stream is finite, `last |> Maybe.withDefault acc` will be the final accumulated value.
-}
fold : (a -> acc -> acc) -> acc -> Stream l a -> Stream l acc
fold f acc s =
    case s of
        Empty e ->
            Empty e

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
filter : (a -> Bool) -> Stream l a -> Stream l a
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
keepIf : (a -> Bool) -> Stream l a -> Stream l a
keepIf =
    filter


{-| Filter the stream based on a mapping function.
-}
filterMap : (a -> Maybe b) -> Stream l a -> Stream l b
filterMap f s =
    case s of
        Empty e ->
            Empty e

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
loop : Stream Finite a -> Stream Infinite a
loop s =
    append s (Immature (\() -> loop s))


flattenF : Stream l (Stream Finite a) -> Stream l a
flattenF s =
    case s of
        Empty e ->
            Empty e

        Mature a rest ->
            append a (Immature (\() -> flattenF rest))

        Immature thunk ->
            flattenF (thunk ())


{-| Flatten a (possibly infinite) stream of (possibly infine) streams into a single stream, in
such a way that we guarantee any indexable element will be processed in a finite number of steps.

For example:

    rangeFrom 0 |> map (\\int -> rangeFrom 0 |> map (\\i -> ( i, int ))) |> omega

will produce a stream of tuples such that `filter ((==) (m,n)) |> head` will return `Just (m,n)`
in a finite number of steps.

-}
flattenN : Stream Infinite (Stream Infinite a) -> Stream Infinite a
flattenN l =
    flattenHelp_ (Flatten_ { untouched = l, touched = [], waiting = [] })


type Flatten_ a
    = Flatten_ { untouched : Stream Infinite (Stream Infinite a), touched : List (Stream Infinite a), waiting : List (Stream Infinite a) }


flattenHelp_ : Flatten_ a -> Stream Infinite a
flattenHelp_ (Flatten_ { untouched, touched, waiting }) =
    case ( untouched, touched, waiting ) of
        -- nothing to do
        ( Empty infinity, _, _ ) ->
            impossible infinity

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


{-| Drop up to `n` values from the stream, so long as there are values to drop.
Dropping any number of values from an empty stream results in an empty stream.
-}
dropN : Int -> Stream l a -> Stream l a
dropN n s =
    if n <= 0 then
        s

    else
        case s of
            Empty e ->
                Empty e

            Mature _ rest ->
                dropN (n - 1) rest

            Immature thunk ->
                Immature (\() -> dropN n (thunk ()))


dropWhile : (a -> Bool) -> Stream l a -> Stream l a
dropWhile f s =
    case s of
        Empty e ->
            Empty e

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
takeN : Int -> Stream l a -> Stream Finite a
takeN n s =
    if n <= 0 then
        empty

    else
        case s of
            Empty _ ->
                empty

            Immature th ->
                Immature (\() -> takeN n (th ()))

            Mature a stream ->
                Mature a (takeN (n - 1) stream)



{- Queries -}


{-| Tests if the stream contains no elements.
-}
isEmpty : Stream Finite a -> Bool
isEmpty s =
    case s of
        Empty _ ->
            True

        Immature thunk ->
            isEmpty (thunk ())

        _ ->
            False


{-| Tests if two streams are equal in terms of content and order of the content.
Will not return for infinite streams that are equal.
-}
equals : Stream l1 a -> Stream l1 a -> Bool
equals s1 s2 =
    case ( s1, s2 ) of
        ( Empty _, Empty _ ) ->
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
member : a -> Stream Finite a -> Bool
member a s =
    case s of
        Empty _ ->
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
length : Stream Finite a -> Int
length s =
    fold (\_ acc -> acc + 1) 0 s |> last |> Maybe.withDefault 0



{- Deconstruct -}


{-| Generate and return the contents of the stream. Will not return for infinite streams.
-}
toList : Stream Finite a -> List a
toList s =
    s |> fold (::) [] |> last |> Maybe.withDefault [] |> List.reverse


{-| Returns the first element of the stream, or `Nothing` if the stream is empty.
-}
head : Stream l a -> Maybe a
head s =
    case s of
        Empty _ ->
            Nothing

        Mature a _ ->
            Just a

        Immature thunk ->
            head (thunk ())


{-| Generates and traverses the stream, returning the last element. Will not return for infinite streams.
-}
last : Stream Finite a -> Maybe a
last s =
    case s of
        Empty _ ->
            Nothing

        Mature a (Empty Finite) ->
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
headThen : Stream l a -> (Stream l a -> Stream l2 a) -> (() -> Stream l2 a) -> Stream l2 a
headThen s whenNotEmpty whenEmpty =
    case s of
        Empty _ ->
            whenEmpty ()

        Mature a sa ->
            Mature a (Immature (\() -> whenNotEmpty sa))

        Immature thunk ->
            headThen (thunk ()) whenNotEmpty whenEmpty
