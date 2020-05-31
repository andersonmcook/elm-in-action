module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)



-- MODEL


type alias Model =
    { page : Page }


type Page
    = Gallery
    | Folders
    | NotFound


initialModel : Model
initialModel =
    { page = Gallery }



-- VIEW


view : Model -> Document Msg
view model =
    let
        content =
            text "This isn't even my final form!"
    in
    { title = "Photo Groove, SPA Style"
    , body = [ lazy viewHeader model.page, content, viewFooter ]
    }


viewLink : { caption : String, targetPage : Page, url : String } -> Page -> Html Msg
viewLink { caption, targetPage, url } page =
    li [ classList [ ( "active", page == targetPage ) ] ]
        [ a [ href url ] [ text caption ]
        ]


viewLinks : Page -> Html Msg
viewLinks page =
    ul []
        [ viewLink { caption = "Folders", targetPage = Folders, url = "/" } page
        , viewLink { caption = "Gallery", targetPage = Gallery, url = "/gallery" } page
        ]


viewHeader : Page -> Html Msg
viewHeader page =
    nav []
        [ h1 [] [ text "Photo Groove" ]
        , viewLinks page
        ]


viewFooter : Html Msg
viewFooter =
    footer [] [ text "'One is never alone with a rubber duck.' - Douglas Adams" ]



-- UPDATE


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = always ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
