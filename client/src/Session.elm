module Session exposing (Session, cacheStory, getCachedStory, navKey, new)

import Browser.Navigation as Navigation


type Session
    = Session Data


new : Navigation.Key -> Session
new navKey_ =
    Session { navKey = navKey_, cachedStory = Nothing }


type alias Data =
    { navKey : Navigation.Key
    , cachedStory : Maybe String
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
