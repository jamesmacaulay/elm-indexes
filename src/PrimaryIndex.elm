module PrimaryIndex
    exposing
        ( PrimaryIndex
        , empty
        , key
        , add
        , removeKey
        , remove
        , get
        , isEmpty
        )

import Dict exposing (Dict)


type PrimaryIndex comparable v
    = PrimaryIndex (v -> comparable) (Dict comparable v)


empty : (v -> comparable) -> PrimaryIndex comparable v
empty f =
    PrimaryIndex f Dict.empty


key : PrimaryIndex comparable v -> v -> comparable
key (PrimaryIndex f _) =
    f


add : v -> PrimaryIndex comparable v -> PrimaryIndex comparable v
add v (PrimaryIndex f d) =
    PrimaryIndex f (Dict.insert (f v) v d)


removeKey : comparable -> PrimaryIndex comparable v -> PrimaryIndex comparable v
removeKey k (PrimaryIndex f d) =
    PrimaryIndex f (Dict.remove k d)


remove : v -> PrimaryIndex comparable v -> PrimaryIndex comparable v
remove v (PrimaryIndex f d) =
    PrimaryIndex f (Dict.remove (f v) d)


get : comparable -> PrimaryIndex comparable v -> Maybe v
get k (PrimaryIndex _ d) =
    Dict.get k d


keys : PrimaryIndex comparable v -> List comparable
keys (PrimaryIndex _ d) =
    Dict.keys d


isEmpty : PrimaryIndex comparable v -> Bool
isEmpty (PrimaryIndex _ d) =
    Dict.isEmpty d
