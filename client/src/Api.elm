module Api exposing (newKey)

import Json.Decode as JD
import RemoteData exposing (WebData)
import RemoteData.Http as Http


api : String
api =
    "https://madlibs.abadi199.workers.dev/"


newKey : (WebData String -> msg) -> Cmd msg
newKey onComplete =
    Http.get api onComplete JD.string
