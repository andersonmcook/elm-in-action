module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MODEL


type alias Photo =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { chosenSize : ThumbnailSize
    , photos : List Photo
    , selectedUrl : String
    }


initialModel : Model
initialModel =
    { chosenSize = Medium
    , photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-- UPDATE


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSurpriseMe ->
            model


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl { url } =
    img
        [ classList [ ( "selected", selectedUrl == url ) ]
        , onClick <| ClickedPhoto <| url
        , src (urlPrefix ++ url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label
        []
        [ input [ name "size", type_ "radio" ] [], text <| sizeToString <| size ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


view : Model -> Html Msg
view { chosenSize, photos, selectedUrl } =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ class <| sizeToString <| chosenSize, id "thumbnails" ] (List.map (viewThumbnail selectedUrl) photos)
        , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
        ]



-- MAIN


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }
