module UniqueIndex
    exposing
        ( UniqueIndex
        , empty
        , key
        , add
        , removeKey
        , remove
        , get
        , isEmpty
        )

import Dict exposing (Dict)


type UniqueIndex comparable v
    = UniqueIndex (v -> comparable) (Dict comparable v)


empty : (v -> comparable) -> UniqueIndex comparable v
empty f =
    UniqueIndex f Dict.empty


key : UniqueIndex comparable v -> v -> comparable
key (UniqueIndex f _) =
    f


add : v -> UniqueIndex comparable v -> UniqueIndex comparable v
add v (UniqueIndex f d) =
    UniqueIndex f (Dict.insert (f v) v d)


removeKey : comparable -> UniqueIndex comparable v -> UniqueIndex comparable v
removeKey k (UniqueIndex f d) =
    UniqueIndex f (Dict.remove k d)


remove : v -> UniqueIndex comparable v -> UniqueIndex comparable v
remove v (UniqueIndex f d) =
    UniqueIndex f (Dict.remove (f v) d)


get : comparable -> UniqueIndex comparable v -> Maybe v
get k (UniqueIndex _ d) =
    Dict.get k d


keys : UniqueIndex comparable v -> List comparable
keys (UniqueIndex _ d) =
    Dict.keys d


isEmpty : UniqueIndex comparable v -> Bool
isEmpty (UniqueIndex _ d) =
    Dict.isEmpty d
