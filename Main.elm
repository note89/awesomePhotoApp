module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (map5, map3, at, int, string)
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
    , photos : WebData (List Photo)
    , limit : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { currentPhoto = RD.NotAsked
            , photos = RD.NotAsked
            , limit = 50
            }
    in
        model ! [ getPhotos model.limit ]


type alias ID =
    Int


type Msg
    = MorePlease
    | GetPhoto ID
    | GetPhotos
    | NewPhotos (WebData (List Photo))
    | NewPhoto (WebData Photo)
    | ScrollEvent ScrollInfo


type alias ScrollInfo =
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int
    }



-- Update


getRandomPhoto : Cmd Msg
getRandomPhoto =
    Random.generate GetPhoto (Random.int 1 5000)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPhoto photoId ->
            ( model, getPhoto photoId )

        GetPhotos ->
            ( model, getPhotos model.limit )

        NewPhotos photos ->
            ( { model | photos = photos }, Cmd.none )

        MorePlease ->
            ( model, getRandomPhoto )

        NewPhoto newPhoto ->
            let
                newModel =
                    { model | currentPhoto = newPhoto }
            in
                ( newModel, Cmd.none )

        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
            -- MAgic number for how far from the bottom we should send a new requests
            if (scrollHeight - scrollTop) <= (offsetHeight + 250) then
                let
                    -- ugly way to keep us from sending mutliple requests.
                    -- might want to keep track of this in the model.
                    -- probably need another state in RemoteData, LoadingMore a
                    -- We have data but still are looking for more.
                    newLimit =
                        case model.photos of
                            RD.Success photos ->
                                if model.limit == List.length photos then
                                    Basics.min (model.limit + 50) 5000
                                else
                                    model.limit

                            _ ->
                                model.limit
                in
                    ( { model | limit = newLimit }, getPhotos newLimit )
            else
                ( model, Cmd.none )



-- VIEW


thumbnailWidth =
    "150px"


thumbnailHeight =
    thumbnailWidth


chillicornLoadingSpinner =
    div [ style [ "width" => thumbnailWidth, "height" => thumbnailHeight, "padding" => "10px", "display" => "flex", "justify-content" => "center", "align-items" => "center" ], class "spin-me" ]
        [ img [ src "/chilicorn_no_text-64.png" ] [] ]


imageHolder : Photo -> Html msg
imageHolder photo =
    div [ style [ "padding" => "10px" ], class "hover-effect" ]
        [ img [ src photo.thumbnailUrl ] [] ]


(=>) =
    (,)


listPhotos model =
    let
        wrapper =
            div [ style [ "display" => "flex", "flex-wrap" => "wrap" ] ]
    in
        case model.photos of
            RD.Success photos ->
                let
                    limitDiff =
                        (model.limit - List.length photos)

                    spinners =
                        List.map (\c -> chillicornLoadingSpinner) (List.range 1 limitDiff)
                in
                    wrapper <| (List.map imageHolder photos) ++ spinners

            _ ->
                wrapper
                    <| List.map (\c -> chillicornLoadingSpinner)
                        (List.range 1 50)


view : Model -> Html Msg
view model =
    div [ style [ "width" => "100%", "height" => "100%", "overflow" => "auto" ], onScroll ScrollEvent ]
        [ node "link" [ rel "stylesheet", href "mystyles.css" ] []
        , div [ style [ "display" => "flex", "justify-content" => "center" ] ] [ h1 [ style [ "color" => "#333" ] ] [ text "Awesome list of pics!" ] ]
        , listPhotos model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Animation.subscription Animate
--     [ model.spinner
--     ]
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


getPhotos : Int -> Cmd Msg
getPhotos limit =
    let
        url =
            "http://jsonplaceholder.typicode.com/photos?_start=0&_limit=" ++ (toString limit)
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


onScroll msg =
    on "scroll" (Decode.map msg scrollInfoDecoder)


scrollInfoDecoder =
    map3 ScrollInfo
        (at [ "target", "scrollHeight" ] int)
        (at [ "target", "scrollTop" ] int)
        (at [ "target", "offsetHeight" ] int)
