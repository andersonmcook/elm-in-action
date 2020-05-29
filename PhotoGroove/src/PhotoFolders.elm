module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = urlPrefix ++ "folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )



-- DECODERS
-- hardcoded for now


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , root =
            Folder
                { expanded = True
                , name = "Photos"
                , photoUrls = []
                , subfolders =
                    [ Folder
                        { expanded = True
                        , name = "2016"
                        , photoUrls = [ "trevi", "coli" ]
                        , subfolders =
                            [ Folder
                                { expanded = True
                                , name = "outdoors"
                                , photoUrls = []
                                , subfolders = []
                                }
                            , Folder
                                { expanded = True
                                , name = "indoors"
                                , photoUrls = [ "fresco" ]
                                , subfolders = []
                                }
                            ]
                        }
                    , Folder
                        { expanded = True
                        , name = "2017"
                        , photoUrls = []
                        , subfolders =
                            [ Folder
                                { expanded = True
                                , name = "outdoors"
                                , photoUrls = []
                                , subfolders = []
                                }
                            , Folder
                                { expanded = True
                                , name = "indoors"
                                , photoUrls = []
                                , subfolders = []
                                }
                            ]
                        }
                    ]
                }
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        }



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
            ( newModel, Cmd.none )

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
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


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
viewFolder path (Folder { expanded, name, subfolders }) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick <| ClickedFolder path ] [ text name ]
    in
    if expanded then
        div [ class "folder expanded" ]
            [ label [ onClick <| ClickedFolder path ] [ text name ]
            , div [ class "contents" ] <| List.indexedMap viewSubfolder subfolders
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



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
