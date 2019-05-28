module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random



-- MODEL


type alias Photo =
    { url : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Errored String
    | Loaded (List Photo) String
    | Loading


type alias Model =
    { chosenSize : ThumbnailSize
    , status : Status
    }


initialModel : Model
initialModel =
    { chosenSize = Medium
    , status = Loading
    }



-- UPDATE


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotPhotos (Result Http.Error String)
    | GotRandomPhoto Photo


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model, Random.generate GotRandomPhoto (Random.uniform firstPhoto otherPhotos) )

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        GotPhotos (Ok response) ->
            case String.split "," response of
                (firstUrl :: _) as urls ->
                    ( { model | status = Loaded (List.map Photo urls) firstUrl }, Cmd.none )

                [] ->
                    ( { model | status = Errored "No photos found." }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )



-- VIEW


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


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label
        []
        [ input
            [ name "size"
            , onClick <| ClickedSize <| size
            , type_ "radio"
            , checked (chosenSize == size)
            ]
            []
        , text <| sizeToString <| size
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ] (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ class <| sizeToString <| chosenSize, id "thumbnails" ] (List.map (viewThumbnail selectedUrl) photos)
    , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
    ]


view : Model -> Html Msg
view { chosenSize, status } =
    div [ class "content" ] <|
        case status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, Cmd.none )
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
