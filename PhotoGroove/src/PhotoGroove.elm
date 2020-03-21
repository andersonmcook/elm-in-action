module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (checked, class, classList, id, name, src, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Random
import Tuple



-- MODEL


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias Model =
    { chosenSize : ThumbnailSize
    , hue : Int
    , noise : Int
    , ripple : Int
    , status : Status
    }


initialModel : Model
initialModel =
    { chosenSize = Large
    , hue = 0
    , noise = 0
    , ripple = 0
    , status = Loading
    }



-- DECODERS


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> Decode.required "url" string
        |> Decode.required "size" int
        |> Decode.optional "title" string "(untitled)"



-- COMMANDS


getPhotos : Cmd Msg
getPhotos =
    Http.get
        { url = urlPrefix ++ "/photos/list.json"
        , expect = Http.expectJson GotPhotos (list photoDecoder)
        }



-- UPDATE


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidNoise Int
    | SlidRipple Int


noOp : Model -> ( Model, Cmd Msg )
noOp model =
    ( model, Cmd.none )


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
                Loaded (head :: tail) _ ->
                    Tuple.pair model <|
                        Random.generate GotRandomPhoto <|
                            Random.uniform head tail

                Loaded [] _ ->
                    noOp model

                Loading ->
                    noOp model

                Errored _ ->
                    noOp model

        GotRandomPhoto { url } ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        GotPhotos (Ok (head :: tail)) ->
            ( { model | status = Loaded (head :: tail) head.url }, Cmd.none )

        GotPhotos (Ok []) ->
            ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )

        SlidHue hue ->
            ( { model | hue = hue }, Cmd.none )

        SlidNoise noise ->
            ( { model | noise = noise }, Cmd.none )

        SlidRipple ripple ->
            ( { model | ripple = ripple }, Cmd.none )



-- VIEW


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl { url } =
    img
        [ classList [ ( "selected", selectedUrl == url ) ]
        , onClick (ClickedPhoto url)
        , src <| urlPrefix ++ url
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size), checked (chosenSize == size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "Small"

        Medium ->
            "Medium"

        Large ->
            "Large"


sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl { chosenSize, hue, noise, ripple } =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ] [ text "Surprise me!" ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" hue
        , viewFilter SlidRipple "Ripple" ripple
        , viewFilter SlidNoise "Noise" noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ] (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToClass chosenSize) ] (List.map (viewThumbnail selectedUrl) photos)
    , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
    ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , node "range-slider"
            [ Attrs.max "11"
            , Attrs.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text <| String.fromInt <| magnitude ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored error ->
                [ text <| "Error: " ++ error ]


main : Program () Model Msg
main =
    Browser.element
        { init = always ( initialModel, getPhotos )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- CUSTOM


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    on "slide" <|
        Json.Decode.map toMsg <|
            at [ "detail", "userSlidTo" ] int
