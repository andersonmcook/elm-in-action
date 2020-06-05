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
    , version : Float
    }



-- VIEW


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                FoldersPage folders ->
                    Html.map GotFoldersMsg <| Folders.view folders

                GalleryPage gallery ->
                    Html.map GotGalleryMsg <| Gallery.view gallery

                NotFound ->
                    text "Not Found"
    in
    { title = "Photo Groove, SPA Style"
    , body = [ lazy viewHeader model.page, content, viewFooter ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { caption = "Folders", url = "/" }
                , navLink Gallery { caption = "Gallery", url = "/gallery" }
                ]

        navLink : Route -> { caption : String, url : String } -> Html Msg
        navLink route { caption, url } =
            li [ classList [ ( "active", isActive { link = route, page = page } ) ] ] [ a [ href url ] [ text caption ] ]
    in
    nav []
        [ logo
        , links
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
    | GotFoldersMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg


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
            updateUrl url model

        GotFoldersMsg foldersMsg ->
            case model.page of
                FoldersPage folders ->
                    toFolders model (Folders.update foldersMsg folders)

                _ ->
                    ( model, Cmd.none )

        GotGalleryMsg galleryMsg ->
            case model.page of
                GalleryPage gallery ->
                    toGallery model (Gallery.update galleryMsg gallery)

                _ ->
                    ( model, Cmd.none )


toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( folders, cmd ) =
    ( { model | page = FoldersPage folders }, Cmd.map GotFoldersMsg cmd )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( gallery, cmd ) =
    ( { model | page = GalleryPage gallery }, Cmd.map GotGalleryMsg cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage gallery ->
            Sub.map GotGalleryMsg <| Gallery.subscriptions gallery

        _ ->
            Sub.none



-- INIT


init : Float -> Url -> Nav.Key -> ( Model, Cmd Msg )
init version url key =
    updateUrl url { page = NotFound, key = key, version = version }


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> string)
        ]


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Folders ->
            toFolders model <| Folders.init Nothing

        Just Gallery ->
            toGallery model <| Gallery.init model.version

        Just (SelectedPhoto filename) ->
            toFolders model <| Folders.init <| Just filename

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )



-- MAIN


main : Program Float Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
