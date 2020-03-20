module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



-- MODEL


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { photos = List.map Photo [ "1.jpeg", "2.jpeg", "3.jpeg" ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Large
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-- UPDATE


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto url ->
            { model | selectedUrl = url }

        ClickedSize size ->
            { model | chosenSize = size }

        ClickedSurpriseMe ->
            { model | selectedUrl = "2.jpeg" }



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


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
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


view : Model -> Html Msg
view { chosenSize, photos, selectedUrl } =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSurpriseMe ] [ text "Surprise me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString chosenSize) ] (List.map (viewThumbnail selectedUrl) photos)
        , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
        ]


main =
    Browser.sandbox
        { init = initialModel
        , update = update
        , view = view
        }



-- module PhotoGroove exposing (main)
-- import Browser
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onClick)
-- import Http
-- import Random
-- -- MODEL
-- type alias Photo =
--     { url : String }
-- type ThumbnailSize
--     = Small
--     | Medium
--     | Large
-- type Status
--     = Errored String
--     | Loaded (List Photo) String
--     | Loading
-- type alias Model =
--     { chosenSize : ThumbnailSize
--     , status : Status
--     }
-- initialModel : Model
-- initialModel =
--     { chosenSize = Medium
--     , status = Loading
--     }
-- -- UPDATE
-- type Msg
--     = ClickedPhoto String
--     | ClickedSize ThumbnailSize
--     | ClickedSurpriseMe
--     | GotPhotos (Result Http.Error String)
--     | GotRandomPhoto Photo
-- selectUrl : String -> Status -> Status
-- selectUrl url status =
--     case status of
--         Loaded photos _ ->
--             Loaded photos url
--         Loading ->
--             status
--         Errored _ ->
--             status
-- update : Msg -> Model -> ( Model, Cmd Msg )
-- update msg model =
--     case msg of
--         ClickedPhoto url ->
--             ( { model | status = selectUrl url model.status }, Cmd.none )
--         ClickedSize size ->
--             ( { model | chosenSize = size }, Cmd.none )
--         ClickedSurpriseMe ->
--             case model.status of
--                 Loaded (firstPhoto :: otherPhotos) _ ->
--                     ( model, Random.generate GotRandomPhoto (Random.uniform firstPhoto otherPhotos) )
--                 Loaded [] _ ->
--                     ( model, Cmd.none )
--                 Loading ->
--                     ( model, Cmd.none )
--                 Errored _ ->
--                     ( model, Cmd.none )
--         GotPhotos (Ok response) ->
--             case String.split "," response of
--                 (firstUrl :: _) as urls ->
--                     ( { model | status = Loaded (List.map Photo urls) firstUrl }, Cmd.none )
--                 [] ->
--                     ( { model | status = Errored "No photos found." }, Cmd.none )
--         GotPhotos (Err _) ->
--             ( { model | status = Errored "Server error!" }, Cmd.none )
--         GotRandomPhoto photo ->
--             ( { model | status = selectUrl photo.url model.status }, Cmd.none )
-- -- VIEW
-- urlPrefix : String
-- urlPrefix =
--     " "
-- viewThumbnail : String -> Photo -> Html Msg
-- viewThumbnail selectedUrl { url } =
--     img
--         [ classList [ ( "selected", selectedUrl == url ) ]
--         , onClick <| ClickedPhoto <| url
--         , src (urlPrefix ++ url)
--         ]
--         []
-- viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
-- viewSizeChooser chosenSize size =
--     label
--         []
--         [ input
--             [ name "size"
--             , onClick <| ClickedSize <| size
--             , type_ "radio"
--             , checked (chosenSize == size)
--             ]
--             []
--         , text <| sizeToString <| size
--         ]
-- sizeToString : ThumbnailSize -> String
-- sizeToString size =
--     case size of
--         Small ->
--             "small"
--         Medium ->
--             "med"
--         Large ->
--             "large"
-- viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
-- viewLoaded photos selectedUrl chosenSize =
--     [ h1 [] [ text "Photo Groove" ]
--     , button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
--     , h3 [] [ text "Thumbnail Size:" ]
--     , div [ id "choose-size" ] (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
--     , div [ class <| sizeToString <| chosenSize, id "thumbnails" ] (List.map (viewThumbnail selectedUrl) photos)
--     , img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl) ] []
--     ]
-- view : Model -> Html Msg
-- view { chosenSize, status } =
--     div [ class "content" ] <|
--         case status of
--             Loaded photos selectedUrl ->
--                 viewLoaded photos selectedUrl chosenSize
--             Loading ->
--                 []
--             Errored errorMessage ->
--                 [ text ("Error: " ++ errorMessage) ]
-- -- MAIN
-- main : Program () Model Msg
-- main =
--     Browser.element
--         { init = always ( initialModel, Cmd.none )
--         , subscriptions = always Sub.none
--         , update = update
--         , view = view
--         }
