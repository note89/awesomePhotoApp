module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (map5, at, int, string)
import RemoteData as RD exposing (WebData)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Photo =
    { albumId : Int
    , id : Int
    , title : String
    , url : String
    , thumbnailUrl : String
    }


type alias Model =
    { currentPhoto : WebData Photo
    }


init : ( Model, Cmd Msg )
init =
    Model RD.NotAsked ! [ getRandomPhoto ]


type Msg
    = MorePlease
    | GetPhoto Int
    | NewPhoto (WebData Photo)


getRandomPhoto : Cmd Msg
getRandomPhoto =
    Random.generate GetPhoto (Random.int 1 5000)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPhoto photoId ->
            ( model, getPhoto photoId )

        MorePlease ->
            ( model, getRandomPhoto )

        NewPhoto newPhoto ->
            let
                newModel =
                    { model | currentPhoto = newPhoto }
            in
                ( newModel, Cmd.none )



-- VIEW


imageHolder : WebData Photo -> Html msg
imageHolder webDataPhoto =
    case webDataPhoto of
        RD.Success photo ->
            div []
                [ img [ src photo.url ] [] ]

        _ ->
            div [] []


view : Model -> Html Msg
view model =
    div []
        [ h2 [] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
          -- , img [ src model.currentPhoto.url ] []
        , imageHolder model.currentPhoto
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


decodePhoto : Decode.Decoder Photo
decodePhoto =
    map5 Photo
        (at [ "albumId" ] int)
        (at [ "id" ] int)
        (at [ "title" ] string)
        (at [ "url" ] string)
        (at [ "thumbnailUrl" ] string)


getPhoto : Int -> Cmd Msg
getPhoto photoId =
    let
        url =
            "http://jsonplaceholder.typicode.com/photos/" ++ (toString photoId)
    in
        Http.get url decodePhoto
            |> RD.sendRequest
            |> Cmd.map NewPhoto
