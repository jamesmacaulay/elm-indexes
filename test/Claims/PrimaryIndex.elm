module Claims.PrimaryIndex exposing (claims)

import Check exposing (..)
import Check.Producer as Producer exposing (Producer)
import PrimaryIndex exposing (PrimaryIndex)


type alias SimpleValue =
    { id : String
    , name : String
    }


simpleEmptyPrimaryIndex : PrimaryIndex String SimpleValue
simpleEmptyPrimaryIndex =
    PrimaryIndex.empty .id


simpleValueID : Producer String
simpleValueID =
    Producer.string


simpleValue : Producer SimpleValue
simpleValue =
    Producer.tuple ( Producer.string, Producer.string )
        |> Producer.map (uncurry SimpleValue)


twoSimpleValuesWithSameID : Producer ( SimpleValue, SimpleValue )
twoSimpleValuesWithSameID =
    Producer.tuple3 ( Producer.string, Producer.string, Producer.string )
        |> Producer.map
            (\( id, name1, name2 ) ->
                ( { id = id, name = name1 }
                , { id = id, name = name2 }
                )
            )


simplePrimaryIndex : Producer (PrimaryIndex String SimpleValue)
simplePrimaryIndex =
    Producer.list simpleValue
        |> Producer.map (List.foldr PrimaryIndex.add simpleEmptyPrimaryIndex)


claims : Claim
claims =
    suite "PrimaryIndex"
        [ claim "adding a value to an empty index results in a non-empty index"
            `that` (\value ->
                        simpleEmptyPrimaryIndex
                            |> PrimaryIndex.add value
                            |> PrimaryIndex.isEmpty
                   )
            `is` (always False)
            `for` simpleValue
        , claim "adding and removing the same value with an empty index results in an empty index"
            `that` (\value ->
                        simpleEmptyPrimaryIndex
                            |> PrimaryIndex.add value
                            |> PrimaryIndex.remove value
                            |> PrimaryIndex.isEmpty
                   )
            `is` (always True)
            `for` simpleValue
        , claim "adding a value means it can be retrieved by its key"
            `that` (\( index, value ) ->
                        index
                            |> PrimaryIndex.add value
                            |> PrimaryIndex.get (PrimaryIndex.key index value)
                   )
            `is` (\( _, value ) -> Just value)
            `for` Producer.tuple ( simplePrimaryIndex, simpleValue )
        , claim "adding a second value with the same key overwrites the first"
            `that` (\( index, ( value1, value2 ) ) ->
                        index
                            |> PrimaryIndex.add value1
                            |> PrimaryIndex.add value2
                            |> PrimaryIndex.get (PrimaryIndex.key index value1)
                   )
            `is` (\( _, ( _, value2 ) ) -> Just value2)
            `for` Producer.tuple ( simplePrimaryIndex, twoSimpleValuesWithSameID )
        , claim "removing a different version of a value with the same key works the same as removing the current version"
            `that` (\( index, ( value1, value2 ) ) ->
                        index
                            |> PrimaryIndex.add value2
                            |> PrimaryIndex.remove value1
                            |> PrimaryIndex.get (PrimaryIndex.key index value2)
                   )
            `is` (always Nothing)
            `for` Producer.tuple ( simplePrimaryIndex, twoSimpleValuesWithSameID )
        ]
