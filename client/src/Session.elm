module Session exposing
    ( Session
    , WindowSize
    , cacheStory
    , device
    , getCachedStory
    , navKey
    , new
    , resizeWindow
    )

import Browser.Navigation as Navigation
import Element as E


type Session
    = Session Data


new : WindowSize -> Navigation.Key -> Session
new windowSize navKey_ =
    Session
        { navKey = navKey_
        , cachedStory = Nothing
        , device = E.classifyDevice windowSize
        }


type alias WindowSize =
    { height : Int, width : Int }


type alias Data =
    { navKey : Navigation.Key
    , cachedStory : Maybe String
    , device : E.Device
    }


navKey : Session -> Navigation.Key
navKey (Session data) =
    data.navKey


cacheStory : String -> Session -> Session
cacheStory story (Session data) =
    Session { data | cachedStory = Just story }


getCachedStory : Session -> Maybe String
getCachedStory (Session data) =
    data.cachedStory


resizeWindow : WindowSize -> Session -> Session
resizeWindow windowSize (Session data) =
    Session { data | device = E.classifyDevice windowSize }


device : Session -> E.Device
device (Session data) =
    data.device
