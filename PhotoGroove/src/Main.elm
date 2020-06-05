module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)



-- MODEL


type Page
    = FoldersPage Folders.Model
    | GalleryPage Gallery.Model
    | NotFound


type Route
    = Folders
    | Gallery
    | SelectedPhoto String


type alias Model =
    { key : Nav.Key
    , page : Page
    }



-- VIEW


view : Model -> Document Msg
view model =
    let
        content =
            text "This isn't even my final form!"
    in
    { title = "Photo Groove, SPA Style"
    , body = [ lazy viewHeader model, content, viewFooter ]
    }


viewLink : Route -> { caption : String, url : String } -> Page -> Html Msg
viewLink route { caption, url } page =
    li [ classList [ ( "active", isActive { link = route, page = page } ) ] ]
        [ a [ href url ] [ text caption ]
        ]


viewLinks : Page -> Route -> Html Msg
viewLinks page route =
    ul []
        [ viewLink route { caption = "Folders", url = "/" } page
        , viewLink route { caption = "Gallery", url = "/gallery" } page
        ]


viewHeader : Model -> Html Msg
viewHeader { page, route } =
    nav []
        [ h1 [] [ text "Photo Groove" ]
        , viewLinks page route
        ]


viewFooter : Html Msg
viewFooter =
    footer [] [ text "'One is never alone with a rubber duck.' - Douglas Adams" ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Folders, FoldersPage _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( Gallery, GalleryPage _ ) ->
            True

        ( Gallery, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, page = urlToPage url }, Cmd.none )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> string)
        ]


urlToPage : Url -> Page
urlToPage url =
    case Parser.parse parser url of
        Just Folders ->
            FoldersPage <| Tuple.first <| Folders.init ()

        Just Gallery ->
            GalleryPage <| Tuple.first <| Gallery.init 1

        Just (SelectedPhoto filename) ->
            FoldersPage <| Tuple.first <| Folders.init ()

        Nothing ->
            NotFound



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
