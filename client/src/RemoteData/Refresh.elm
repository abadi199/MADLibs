module RemoteData.Refresh exposing
    ( RefreshableRemoteData(..)
    , fromRemoteData
    , map
    , refresh
    , withDefault
    )

import Http
import RemoteData


type RefreshableRemoteData data
    = NotAsked
    | Loading
    | Reloading data
    | Failure Http.Error
    | FailureWithData Http.Error data
    | Success data


map : (a -> b) -> RefreshableRemoteData a -> RefreshableRemoteData b
map f remoteData =
    case remoteData of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Reloading data ->
            Reloading (f data)

        Failure error ->
            Failure error

        FailureWithData error data ->
            FailureWithData error (f data)

        Success data ->
            Success (f data)


fromRemoteData : RemoteData.WebData data -> RefreshableRemoteData data
fromRemoteData remoteData =
    case remoteData of
        RemoteData.NotAsked ->
            NotAsked

        RemoteData.Loading ->
            Loading

        RemoteData.Failure error ->
            Failure error

        RemoteData.Success data ->
            Success data


refresh : RemoteData.WebData data -> RefreshableRemoteData data -> RefreshableRemoteData data
refresh upcoming rrd =
    case upcoming of
        RemoteData.NotAsked ->
            rrd

        RemoteData.Loading ->
            case rrd |> toData of
                Nothing ->
                    Loading

                Just data ->
                    Reloading data

        RemoteData.Failure error ->
            case rrd |> toData of
                Nothing ->
                    Failure error

                Just data ->
                    FailureWithData error data

        RemoteData.Success data ->
            Success data


toData : RefreshableRemoteData data -> Maybe data
toData rrd =
    case rrd of
        NotAsked ->
            Nothing

        Loading ->
            Nothing

        Reloading data ->
            Just data

        Failure _ ->
            Nothing

        FailureWithData _ data ->
            Just data

        Success data ->
            Just data


withDefault : data -> RefreshableRemoteData data -> data
withDefault default rrd =
    case rrd of
        NotAsked ->
            default

        Loading ->
            default

        Reloading data ->
            data

        Failure _ ->
            default

        FailureWithData _ data ->
            data

        Success data ->
            data
