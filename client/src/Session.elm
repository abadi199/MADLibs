module Session exposing (Session, navKey, new)

import Browser.Navigation as Navigation


type Session
    = Session Data


new : Navigation.Key -> Session
new navKey_ =
    Session { navKey = navKey_ }


type alias Data =
    { navKey : Navigation.Key }


navKey : Session -> Navigation.Key
navKey (Session data) =
    data.navKey
