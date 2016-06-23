module SecondarySetIndex
    exposing
        ( SecondarySetIndex
        , empty
        , insert
        , isEmpty
        , primaryKey
        , secondaryKey
        , get
        )

import Dict exposing (Dict)
import Set exposing (Set)


-- type SecondarySetIndex primaryKey secondaryKey value


type SecondarySetIndex comparable comparable' value
    = SecondarySetIndex (value -> comparable) (value -> Set comparable') (Dict comparable' (Set comparable)) (Dict comparable (Set comparable'))


empty : (value -> comparable) -> (value -> Set comparable') -> SecondarySetIndex comparable comparable' value
empty pf sf =
    SecondarySetIndex pf sf Dict.empty Dict.empty


primaryKey : SecondarySetIndex comparable comparable' value -> (value -> comparable)
primaryKey (SecondarySetIndex pf _ _ _) =
    pf


secondaryKey : SecondarySetIndex comparable comparable' value -> (value -> Set comparable')
secondaryKey (SecondarySetIndex _ sf _ _) =
    sf


get : comparable' -> SecondarySetIndex comparable comparable' value -> Set comparable
get secondaryKey (SecondarySetIndex _ _ primaryKeysBySecondaryKey _) =
    Dict.get secondaryKey primaryKeysBySecondaryKey
        |> Maybe.withDefault Set.empty


insert : value -> SecondarySetIndex comparable comparable' value -> SecondarySetIndex comparable comparable' value
insert v (SecondarySetIndex pf sf primaryKeysBySecondaryKey secondaryKeysByPrimaryKey) =
    let
        pk =
            pf v

        sks =
            sf v

        previousSKs =
            Dict.get pk secondaryKeysByPrimaryKey
                |> Maybe.withDefault Set.empty

        newSKs =
            Set.diff sks previousSKs

        oldSKs =
            Set.diff previousSKs sks

        updateToAddPrimaryKey mpks =
            case mpks of
                Nothing ->
                    Just (Set.singleton pk)

                Just pks ->
                    Just (Set.insert pk pks)

        folderToAddPrimaryKey sk =
            Dict.update sk updateToAddPrimaryKey

        primaryKeysBySecondaryKey' =
            Set.foldr folderToAddPrimaryKey primaryKeysBySecondaryKey newSKs

        updateToRemovePrimaryKey mpks =
            case mpks of
                Nothing ->
                    Nothing

                Just pks ->
                    let
                        pks' =
                            Set.remove pk pks
                    in
                        if Set.isEmpty pks' then
                            Nothing
                        else
                            Just pks'

        folderToRemovePrimaryKey sk =
            Dict.update sk updateToRemovePrimaryKey

        primaryKeysBySecondaryKey'' =
            Set.foldr folderToRemovePrimaryKey primaryKeysBySecondaryKey' oldSKs

        secondaryKeysByPrimaryKey' =
            if Set.isEmpty sks then
                Dict.remove pk secondaryKeysByPrimaryKey
            else
                Dict.insert pk sks secondaryKeysByPrimaryKey
    in
        SecondarySetIndex pf sf primaryKeysBySecondaryKey'' secondaryKeysByPrimaryKey'


isEmpty : SecondarySetIndex comparable comparable' value -> Bool
isEmpty (SecondarySetIndex _ _ d1 d2) =
    Dict.isEmpty d1 && Dict.isEmpty d2


secondaryKeys : SecondarySetIndex comparable comparable' value -> List comparable'
secondaryKeys (SecondarySetIndex _ _ primaryKeysBySecondaryKey _) =
    Dict.keys primaryKeysBySecondaryKey


primaryKeys : SecondarySetIndex comparable comparable' value -> List comparable
primaryKeys (SecondarySetIndex _ _ _ secondaryKeysByPrimaryKey) =
    Dict.keys secondaryKeysByPrimaryKey
