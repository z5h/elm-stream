module TypedTests exposing (suite)

import Array
import Expect exposing (Expectation, equal)
import Stream.Typed as Stream exposing (Finite, Stream)
import Test exposing (..)


expect : Bool -> Expectation
expect val =
    val |> equal True


expectNot : Bool -> Expectation
expectNot val =
    val |> equal False


makeLazy : Stream length a -> Stream length a
makeLazy s =
    Stream.lazy (\() -> s)


streams :
    { empty : Stream finite a
    , singleton : Stream finite String
    , abc : Stream finite String
    , xyz : Stream finite String
    , naturalsRange : Stream finite Int
    , binary : Stream finite Int
    , constant : Stream finite String
    }
streams =
    { empty =
        Stream.empty
    , singleton =
        Stream.singleton "s"
    , abc =
        Stream.fromList [ "a", "b", "c" ]
    , xyz =
        Stream.fromList [ "x", "y", "z" ]
    , naturalsRange =
        Stream.rangeFrom 1
    , binary = Stream.fromList [ 0, 1 ]
    , constant =
        Stream.constant "k"
    }


{-| note that if you find yourself sorting a stream, you probably aren't solving your problem in the best way
here we use it as a quick & dirty hack for testing purposes
-}
sort : Stream Finite comparable -> Stream Finite comparable
sort s =
    s |> Stream.toList |> List.sort |> Stream.fromList


toMaybes : Stream length a -> Stream length (Maybe a)
toMaybes s =
    Stream.append (Stream.map Just s) (Stream.constant Nothing)


expectStreamsEqual : Stream length a -> Stream length a -> Expectation
expectStreamsEqual s1 s2 =
    let
        firstDifference =
            Stream.zip (toMaybes s1) (toMaybes s2)
                |> Stream.dropWhile (\( a, b ) -> a == b && a /= Nothing)
                |> Stream.head
    in
    case firstDifference of
        Just ( Just a, Just b ) ->
            Expect.fail <| "Expected " ++ Debug.toString a ++ " but found " ++ Debug.toString b

        Just ( Just a, Nothing ) ->
            Expect.fail <| "Expected " ++ Debug.toString a ++ " but found end of stream"

        Just ( Nothing, Just b ) ->
            Expect.fail <| "Expected end of stream but found " ++ Debug.toString b

        _ ->
            Expect.pass


type Triple a b c
    = Triple a b c


creationTests : Test
creationTests =
    describe "Creation"
        [ -- empty
          describe "empty stream"
            [ test "isEmpty" <|
                \_ ->
                    expect (Stream.isEmpty streams.empty)
            , test "has no head" <|
                \_ ->
                    Stream.head streams.empty
                        |> Expect.equal Nothing
            , test "has length 0" <|
                \_ ->
                    Stream.length streams.empty |> Expect.equal 0
            , test "has no last" <|
                \_ ->
                    Stream.last streams.empty
                        |> Expect.equal Nothing
            , test "maps" <|
                \_ ->
                    Stream.map (always []) streams.empty
                        |> Expect.equal streams.empty
            ]

        -- singleton
        , describe "singleton stream"
            [ test "is not empty" <|
                \_ ->
                    expectNot (Stream.isEmpty streams.singleton)
            , test "has head" <|
                \_ ->
                    Stream.head streams.singleton
                        |> Expect.equal (Just "s")
            , test "has length 1" <|
                \_ ->
                    Stream.length streams.singleton |> Expect.equal 1
            , test "has last" <|
                \_ ->
                    Stream.last streams.singleton
                        |> Expect.equal (Just "s")
            ]

        -- constant
        , describe "constant"
            [ test "can dropN 1000 and takeN 2" <|
                \_ ->
                    streams.constant
                        |> Stream.dropN 100
                        |> Stream.takeN 2
                        |> expectStreamsEqual
                            (Stream.fromList [ "k", "k" ])
            ]

        -- lazy
        , describe "lazy"
            [ test "lazy empty is the same as empty" <|
                \_ ->
                    Stream.lazy (\_ -> streams.empty)
                        |> expectStreamsEqual streams.empty
            , test "lazy (lazy abc) is the same as abc" <|
                \_ ->
                    (\_ ->
                        (\_ -> streams.abc)
                            |> Stream.lazy
                    )
                        |> Stream.lazy
                        |> expectStreamsEqual streams.abc
            ]

        -- range
        , describe "range"
            [ test "range -2 2 should contain inlcusive values" <|
                \_ ->
                    Stream.range -2 2
                        |> expectStreamsEqual
                            (Stream.fromList [ -2, -1, 0, 1, 2 ])
            , test "range 3 3 should contain single value" <|
                \_ ->
                    Stream.range 3 3
                        |> expectStreamsEqual
                            (Stream.fromList [ 3 ])
            , test "range 5 0 should be empty" <|
                \_ ->
                    Stream.range 5 0
                        |> expectStreamsEqual
                            Stream.empty
            ]
        , --rangeFrom
          describe "rangeFrom"
            [ test "naturals contains first naturals" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.takeN 3
                        |> expectStreamsEqual
                            (Stream.fromList [ 1, 2, 3 ])
            , test "naturals contains larger naturals" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.member 1000
                        |> Expect.equal True
            ]

        -- fromGenerator
        , describe "fromGenerator" <|
            [ test "generates geometric sequence" <|
                \_ ->
                    1
                        |> Stream.fromGenerator
                            (\n ->
                                if n < 128 then
                                    Just (2 * n)

                                else
                                    Nothing
                            )
                        |> expectStreamsEqual
                            (Stream.fromList [ 1, 2, 4, 8, 16, 32, 64, 128 ])
            ]
        ]


createFromDataStructures : Test
createFromDataStructures =
    describe "Create from other data structures"
        [ -- fromList
          describe "fromList"
            [ test "of empty list" <|
                \_ ->
                    Stream.fromList []
                        |> expectStreamsEqual streams.empty
            , test "of abc" <|
                \_ ->
                    Stream.fromList [ "a", "b", "c" ]
                        |> expectStreamsEqual streams.abc
            ]
        , describe "fromArray"
            [ test "of empty array" <|
                \_ ->
                    Stream.fromArray (Array.fromList [])
                        |> expectStreamsEqual streams.empty
            , test "of a,b,c" <|
                \_ ->
                    Stream.fromArray (Array.fromList [ "a", "b", "c" ])
                        |> expectStreamsEqual streams.abc
            ]
        ]


combineTests : Test
combineTests =
    describe "combine"
        [ -- append
          describe "append"
            [ test "appends abc and xyz" <|
                \_ ->
                    Stream.append streams.abc streams.xyz
                        |> expectStreamsEqual
                            (Stream.fromList [ "a", "b", "c", "x", "y", "z" ])
            , test "appends abc and empty" <|
                \_ ->
                    Stream.append streams.abc streams.empty
                        |> expectStreamsEqual streams.abc
            , test "appends empty and xyz" <|
                \_ ->
                    Stream.append streams.empty streams.xyz
                        |> Stream.takeN 3
                        |> expectStreamsEqual streams.xyz
            , test "appends constant and abc" <|
                \_ ->
                    Stream.append streams.constant streams.abc
                        |> Stream.dropN 100
                        |> Stream.head
                        |> Expect.equal (Just "k")
            ]
        , describe "product" <|
            [ test "works on empty and singleton streams" <|
                \_ ->
                    Stream.product streams.empty streams.singleton
                        |> Stream.toList
                        |> Expect.equal []
            , test "works on singleton and empty streams" <|
                \_ ->
                    Stream.product streams.singleton streams.empty
                        |> Stream.toList
                        |> Expect.equal []
            , test "works on finite streams " <|
                \_ ->
                    Stream.product streams.abc streams.xyz
                        |> Stream.toList
                        |> List.sort
                        |> Expect.equal
                            [ ( "a", "x" )
                            , ( "a", "y" )
                            , ( "a", "z" )
                            , ( "b", "x" )
                            , ( "b", "y" )
                            , ( "b", "z" )
                            , ( "c", "x" )
                            , ( "c", "y" )
                            , ( "c", "z" )
                            ]
            , test "works on finite and infinite stream" <|
                \_ ->
                    let
                        expectedItems =
                            [ ( "a", 1 )
                            , ( "a", 2 )
                            , ( "a", 3 )
                            , ( "b", 1 )
                            , ( "b", 2 )
                            , ( "b", 3 )
                            , ( "c", 1 )
                            , ( "c", 2 )
                            , ( "c", 3 )
                            ]
                    in
                    Stream.product streams.abc streams.naturalsRange
                        |> Stream.filter
                            (\t -> List.member t expectedItems)
                        |> Stream.takeN 9
                        |> Stream.toList
                        |> List.sort
                        |> Expect.equal expectedItems
            , test "works on infinite and finite streams" <|
                \_ ->
                    let
                        expectedItems =
                            [ ( 1, "a" )
                            , ( 1, "b" )
                            , ( 1, "c" )
                            , ( 2, "a" )
                            , ( 2, "b" )
                            , ( 2, "c" )
                            , ( 3, "a" )
                            , ( 3, "b" )
                            , ( 3, "c" )
                            ]
                    in
                    Stream.product streams.naturalsRange streams.abc
                        |> Stream.filter
                            (\t -> List.member t expectedItems)
                        |> Stream.takeN 9
                        |> Stream.toList
                        |> List.sort
                        |> Expect.equal expectedItems
            ]
        , describe "zip"
            [ test "zips abc and xyz" <|
                \_ ->
                    Stream.zip streams.abc streams.xyz
                        |> Stream.takeN 3
                        |> expectStreamsEqual
                            (Stream.fromList [ ( "a", "x" ), ( "b", "y" ), ( "c", "z" ) ])
            , test "zips abc and empty" <|
                \_ ->
                    Stream.zip streams.abc streams.empty
                        |> expectStreamsEqual streams.empty
            , test "zips empty and xyz" <|
                \_ ->
                    Stream.zip streams.empty streams.xyz
                        |> expectStreamsEqual streams.empty
            , test "zips abc and onetwo" <|
                \_ ->
                    Stream.zip streams.abc streams.binary
                        |> Stream.takeN 2
                        |> expectStreamsEqual
                            (Stream.fromList [ ( "a", 0 ), ( "b", 1 ) ])
            ]
        , describe "apply"
            [ test "applies a function to a stream" <|
                \_ ->
                    let
                        triplesStream =
                            Stream.singleton Triple
                                |> Stream.apply streams.binary
                                |> Stream.apply streams.binary
                                |> Stream.apply streams.binary
                                |> Stream.map (\(Triple a b c) -> [ a, b, c ])
                                |> sort
                    in
                    triplesStream
                        |> expectStreamsEqual
                            (Stream.fromList
                                [ [ 0, 0, 0 ]
                                , [ 0, 0, 1 ]
                                , [ 0, 1, 0 ]
                                , [ 0, 1, 1 ]
                                , [ 1, 0, 0 ]
                                , [ 1, 0, 1 ]
                                , [ 1, 1, 0 ]
                                , [ 1, 1, 1 ]
                                ]
                            )
            ]
        ]


transformTests : Test
transformTests =
    describe "transform"
        [ -- map
          describe "map"
            [ test "doubles naturals" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.map ((*) 2)
                        |> Stream.takeN 3
                        |> expectStreamsEqual
                            (Stream.fromList [ 2, 4, 6 ])
            ]

        -- concatMap
        , describe "concatMap"
            [ test "repeats each natural n n times" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.concatMap
                            (\x ->
                                List.repeat x x |> Stream.fromList
                            )
                        |> Stream.takeN 7
                        |> expectStreamsEqual
                            (Stream.fromList [ 1, 2, 2, 3, 3, 3, 4 ])
            ]

        -- filter (keepIf)
        , describe "filter"
            [ test "filters naturals" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.filter (\x -> modBy 5 x == 0)
                        |> Stream.takeN 3
                        |> expectStreamsEqual
                            (Stream.fromList [ 5, 10, 15 ])
            ]

        -- filterMap
        , describe "filterMap"
            [ test "nothings don't count" <|
                \_ ->
                    streams.abc
                        |> Stream.filterMap (\_ -> Nothing)
                        |> expectStreamsEqual streams.empty
            , test "justs are kept" <|
                \_ ->
                    streams.abc
                        |> Stream.filterMap Just
                        |> expectStreamsEqual streams.abc
            ]

        -- takeN
        , describe "takeN"
            [ test "takes 2 from abc" <|
                \_ ->
                    streams.abc
                        |> Stream.takeN 2
                        |> expectStreamsEqual (Stream.fromList [ "a", "b" ])
            , test "takes 2 from infinite " <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.takeN 2
                        |> expectStreamsEqual (Stream.fromList [ 1, 2 ])
            ]
        , describe "dropN"
            [ test "drops 2 from abc" <|
                \_ ->
                    streams.abc
                        |> Stream.dropN 2
                        |> expectStreamsEqual (Stream.fromList [ "c" ])
            , test "drops 2 from infinite " <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.dropN 2
                        |> Stream.takeN 2
                        |> expectStreamsEqual (Stream.fromList [ 3, 4 ])
            ]
        , describe "takeWhile"
            [ test "takes while < 4" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.takeWhile (\n -> n < 4)
                        |> expectStreamsEqual (Stream.fromList [ 1, 2, 3 ])
            ]
        , describe "dropWhile"
            [ test "drops while < 4" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.dropWhile (\n -> n < 4)
                        |> Stream.takeN 2
                        |> expectStreamsEqual (Stream.fromList [ 4, 5 ])
            ]

        -- flatten
        , describe "flatten"
            [ test "flattens infinite list of infinite lists of naturals" <|
                \_ ->
                    streams.naturalsRange
                        |> Stream.map
                            (\x ->
                                streams.naturalsRange
                                    |> Stream.map (\y -> ( x, y ))
                            )
                        |> Stream.flatten
                        |> Stream.member ( 101, 102 )
                        |> Expect.equal True
            ]
        , -- loop
          describe "loop"
            [ test "loops abc" <|
                \_ ->
                    Stream.loop streams.abc
                        |> Stream.takeN 7
                        |> expectStreamsEqual
                            (Stream.fromList [ "a", "b", "c", "a", "b", "c", "a" ])
            , test "loops empty" <|
                \_ ->
                    Stream.loop streams.empty
                        |> expectStreamsEqual streams.empty
            , test "loops lazy abc" <|
                \_ ->
                    Stream.loop (makeLazy streams.abc)
                        |> Stream.takeN 7
                        |> expectStreamsEqual
                            (Stream.fromList [ "a", "b", "c", "a", "b", "c", "a" ])
            ]
        ]


queriesTests : Test
queriesTests =
    describe "queries"
        [ describe "length"
            [ test "of empty stream" <|
                \_ ->
                    Stream.length streams.empty
                        |> Expect.equal 0
            , test "of abc" <|
                \_ ->
                    Stream.length streams.abc
                        |> Expect.equal 3
            , test "of lazy abc" <|
                \_ ->
                    Stream.length (makeLazy streams.abc)
                        |> Expect.equal 3
            ]
        , describe "equals" <|
            [ test "empty streams are equal" <|
                \_ ->
                    Stream.equals streams.empty streams.empty
                        |> Expect.equal True
            , test "empty and singleton streams are not equal" <|
                \_ ->
                    Stream.equals streams.empty streams.singleton
                        |> Expect.equal False
            , test "abc and abc are equal" <|
                \_ ->
                    Stream.equals streams.abc streams.abc
                        |> Expect.equal True
            , test "abc and lazy abc are equal" <|
                \_ ->
                    Stream.equals streams.abc (makeLazy streams.abc)
                        |> Expect.equal True
            ]
        , describe "member"
            [ test "abc contains b" <|
                \_ ->
                    Stream.member "b" streams.abc
                        |> Expect.equal True
            , test "abc does not contain x" <|
                \_ ->
                    Stream.member "x" streams.abc
                        |> Expect.equal False
            , test "naturals contains 101" <|
                \_ ->
                    Stream.member 101 streams.naturalsRange
                        |> Expect.equal True
            ]
        ]


deconstructTests : Test
deconstructTests =
    describe "deconstruct"
        [ describe "isEmpty"
            [ test "of empty stream" <|
                \_ ->
                    Stream.isEmpty streams.empty
                        |> Expect.equal True
            , test "of abc" <|
                \_ ->
                    Stream.isEmpty streams.abc
                        |> Expect.equal False
            ]
        , describe "head"
            [ test "of empty stream" <|
                \_ ->
                    Stream.head streams.empty
                        |> Expect.equal Nothing
            , test "of abc" <|
                \_ ->
                    Stream.head streams.abc
                        |> Expect.equal (Just "a")
            , test "of lazy abc" <|
                \_ ->
                    Stream.head (makeLazy streams.abc)
                        |> Expect.equal (Just "a")
            ]
        , describe "last"
            [ test "of empty stream" <|
                \_ ->
                    Stream.last streams.empty
                        |> Expect.equal Nothing
            , test "of abc" <|
                \_ ->
                    Stream.last streams.abc
                        |> Expect.equal (Just "c")
            , test "of lazy abc" <|
                \_ ->
                    Stream.last (makeLazy streams.abc)
                        |> Expect.equal (Just "c")
            ]
        , -- toList
          describe "toList"
            [ test "of empty stream" <|
                \_ ->
                    Stream.toList streams.empty
                        |> Expect.equal []
            , test "of abc" <|
                \_ ->
                    Stream.toList streams.abc
                        |> Expect.equal [ "a", "b", "c" ]
            ]
        ]


suite : Test
suite =
    describe "Stream - Typed"
        [ creationTests
        , createFromDataStructures
        , combineTests
        , transformTests
        , queriesTests
        , deconstructTests
        ]
