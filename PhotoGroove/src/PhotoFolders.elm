module PhotoFolders exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Html exposing (Html, a, div, h2, h3, img, label, span, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)



-- MODEL


type alias Photo =
    { relatedUrls : List String
    , size : Int
    , title : String
    , url : String
    }


type alias JsonPhoto =
    { relatedUrls : List String
    , size : Int
    , title : String
    }


type Folder
    = Folder
        { expanded : Bool
        , name : String
        , photoUrls : List String
        , subfolders : List Folder
        }


type alias Model =
    { photos : Dict String Photo
    , root : Folder
    , selectedPhotoUrl : Maybe String
    }


initialModel : Model
initialModel =
    { photos = Dict.empty
    , root =
        Folder
            { expanded = False
            , name = "Loading..."
            , photoUrls = []
            , subfolders = []
            }
    , selectedPhotoUrl = Nothing
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



-- INIT


init : Maybe String -> ( Model, Cmd Msg )
init selectedPhotoUrl =
    ( { initialModel | selectedPhotoUrl = selectedPhotoUrl }
    , Http.get
        { url = urlPrefix ++ "folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )



-- DECODERS


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { relatedUrls = json.relatedUrls
      , size = json.size
      , title = json.title
      , url = url
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs =
    Dict.fromList << List.map finishPhoto


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "related_photos" (list string)
        |> required "size" int
        |> required "title" string


photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.map fromPairs <| Decode.keyValuePairs jsonPhotoDecoder


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list folderDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { expanded = True
        , name = name
        , photoUrls = Dict.keys photos
        , subfolders = subfolders
        }


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos" photosDecoder
        |> required "subfolders" (Decode.lazy (\_ -> list modelPhotosDecoder))


modelPhotosFromJson : Dict String Photo -> List (Dict String Photo) -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos
            , root = root
            , selectedPhotoUrl = Nothing
            }
        )
        modelPhotosDecoder
        folderDecoder



-- UPDATE


type FolderPath
    = End
    | Subfolder Int FolderPath


type Msg
    = ClickedFolder FolderPath
    | ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )

        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( { newModel | selectedPhotoUrl = model.selectedPhotoUrl }, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ] [ viewFolder End model.root ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewPhoto : String -> Html Msg
viewPhoto url =
    a [ class "photo", href ("/photos/" ++ url), onClick <| ClickedPhoto url ] [ text url ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src <| String.join "" [ urlPrefix, "photos/", photo.url, "/full" ] ]
            []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ] <| List.map viewRelatedPhoto photo.relatedUrls
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick <| ClickedPhoto url
        , src <| String.join "" [ urlPrefix, "photos/", url, "/thumb" ]
        ]
        []


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder { expanded, name, photoUrls, subfolders }) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel : Html Msg
        folderLabel =
            label [ onClick <| ClickedFolder path ] [ text name ]

        contents : List (Html Msg)
        contents =
            List.append (List.indexedMap viewSubfolder subfolders) (List.map viewPhoto photoUrls)
    in
    if expanded then
        div [ class "folder expanded" ]
            [ label [ onClick <| ClickedFolder path ] [ text name ]
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)
