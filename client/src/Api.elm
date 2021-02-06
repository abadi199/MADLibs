module Api exposing (load, newKey, save)

import Json.Decode as JD
import Json.Encode as JE
import RemoteData exposing (WebData)
import RemoteData.Http as Http


api : String
api =
    "https://madlibs.abadi199.workers.dev/"


newKey : (WebData String -> msg) -> Cmd msg
newKey onComplete =
    Http.get api onComplete JD.string


load : String -> (WebData String -> msg) -> Cmd msg
load key onComplete =
    Http.get (api ++ key) onComplete JD.string


save : { a | key : String, text : String } -> (WebData () -> msg) -> Cmd msg
save { key, text } onComplete =
    Http.put (api ++ key) onComplete (JD.succeed ()) (JE.string text)
