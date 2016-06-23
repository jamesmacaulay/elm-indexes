module Claims.UniqueIndex exposing (..)

import Check exposing (..)
import Check.Producer as Producer exposing (Producer)
import UniqueIndex exposing (UniqueIndex)


type alias SimpleValue =
    { id : String
    , name : String
    }


simpleEmptyUniqueIndex : UniqueIndex String SimpleValue
simpleEmptyUniqueIndex =
    UniqueIndex.empty .id


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


simpleUniqueIndex : Producer (UniqueIndex String SimpleValue)
simpleUniqueIndex =
    Producer.list simpleValue
        |> Producer.map (List.foldr UniqueIndex.add simpleEmptyUniqueIndex)


claims : Claim
claims =
    suite "UniqueIndex"
        [ claim "adding a value to an empty index results in a non-empty index"
            `that` (\value ->
                        simpleEmptyUniqueIndex
                            |> UniqueIndex.add value
                            |> UniqueIndex.isEmpty
                   )
            `is` (always False)
            `for` simpleValue
        , claim "adding and removing the same value with an empty index results in an empty index"
            `that` (\value ->
                        simpleEmptyUniqueIndex
                            |> UniqueIndex.add value
                            |> UniqueIndex.remove value
                            |> UniqueIndex.isEmpty
                   )
            `is` (always True)
            `for` simpleValue
        , claim "adding a value means it can be retrieved by its key"
            `that` (\( index, value ) ->
                        index
                            |> UniqueIndex.add value
                            |> UniqueIndex.get (UniqueIndex.key index value)
                   )
            `is` (\( _, value ) -> Just value)
            `for` Producer.tuple ( simpleUniqueIndex, simpleValue )
        , claim "adding a second value with the same key overwrites the first"
            `that` (\( index, ( value1, value2 ) ) ->
                        index
                            |> UniqueIndex.add value1
                            |> UniqueIndex.add value2
                            |> UniqueIndex.get (UniqueIndex.key index value1)
                   )
            `is` (\( _, ( _, value2 ) ) -> Just value2)
            `for` Producer.tuple ( simpleUniqueIndex, twoSimpleValuesWithSameID )
        , claim "removing a different version of a value with the same key works the same as removing the current version"
            `that` (\( index, ( value1, value2 ) ) ->
                        index
                            |> UniqueIndex.add value2
                            |> UniqueIndex.remove value1
                            |> UniqueIndex.get (UniqueIndex.key index value2)
                   )
            `is` (always Nothing)
            `for` Producer.tuple ( simpleUniqueIndex, twoSimpleValuesWithSameID )
        ]
