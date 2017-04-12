module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (map5, at, int, string)
import RemoteData as RD exposing (WebData)
import Random
import Animation exposing (px, turn, percent)


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
    , spinner : Animation.State
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { currentPhoto = RD.NotAsked
            , spinner = beginInitialSpin <| Animation.style [ Animation.rotate (turn 0) ]
            }
    in
        model ! [ getRandomPhoto ]


beginInitialSpin spinner =
    Animation.interrupt
        [ Animation.loop
            [ Animation.toWith (Animation.speed { perSecond = 4 })
                [ Animation.rotate (turn 1) ]
            , Animation.set [ Animation.rotate (turn 0) ]
            ]
        ]
        spinner


type alias ID =
    Int


type Msg
    = MorePlease
    | GetPhoto ID
    | GetPhotos
    | NewPhotos (WebData (List Photo))
    | NewPhoto (WebData Photo)
    | Animate Animation.Msg


getRandomPhoto : Cmd Msg
getRandomPhoto =
    Random.generate GetPhoto (Random.int 1 5000)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPhoto photoId ->
            ( model, getPhoto photoId )

        GetPhotos ->
            ( model, getPhotos )

        NewPhotos photos ->
            ( model, Cmd.none )

        MorePlease ->
            ( model, getRandomPhoto )

        NewPhoto newPhoto ->
            let
                newModel =
                    { model | currentPhoto = newPhoto }
            in
                ( newModel, Cmd.none )

        Animate animMsg ->
            ( { model
                | spinner = Animation.update animMsg model.spinner
              }
            , Cmd.none
            )



-- VIEW


chillicornLoadingSpinner spinner =
    div []
        [ img (Animation.render spinner ++ [ src "/chilicorn_no_text-256.png" ]) [] ]


imageHolder : Model -> Html msg
imageHolder model =
    case model.currentPhoto of
        RD.Success photo ->
            div []
                [ img [ src photo.url ] [] ]

        _ ->
            chillicornLoadingSpinner model.spinner


view : Model -> Html Msg
view model =
    div []
        [ h2 [] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        , br [] []
        , imageHolder model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate
        [ model.spinner
        ]



-- HTTP


decodePhoto : Decode.Decoder Photo
decodePhoto =
    map5 Photo
        (at [ "albumId" ] int)
        (at [ "id" ] int)
        (at [ "title" ] string)
        (at [ "url" ] string)
        (at [ "thumbnailUrl" ] string)


decodePhotos : Decode.Decoder (List Photo)
decodePhotos =
    Decode.list decodePhoto


getPhotos : Cmd Msg
getPhotos =
    let
        url =
            "http://jsonplaceholder.typicode.com/photos?_start=0&_limit=50"
    in
        Http.get url decodePhotos
            |> RD.sendRequest
            |> Cmd.map NewPhotos


getPhoto : Int -> Cmd Msg
getPhoto photoId =
    let
        url =
            "http://jsonplaceholder.typicode.com/photos/" ++ (toString photoId)
    in
        Http.get url decodePhoto
            |> RD.sendRequest
            |> Cmd.map NewPhoto
