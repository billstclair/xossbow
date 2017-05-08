----------------------------------------------------------------------
--
-- TestBackend.elm
-- Test UI for Xossbow backends.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module TestBackend exposing (..)

import Xossbow.Types as Types
    exposing ( UploadType(..), Authorization
             , BackendOperation, BackendWrapper, Backend, BackendResult
             , BackendError(..)
             , authorize_, uploadFile, downloadFile, updateStateFromResult
             )
import Xossbow.Backend.RamDict as RamDict
import Xossbow.Backend.ApachePost as ApachePost

import Html exposing ( Html, Attribute
                     , div, p, text, input, select, option, button
                     )
import Html.Attributes as Attributes
    exposing ( type_, style, href, rows, cols, class, value, selected )
import Html.Events exposing ( onClick, onInput )

log = Debug.log

backends : List (String, Backend Msg)
backends =
    [ ("RamDict", RamDict.backend)
    , ("ApachePost", ApachePost.backend)
    ]

stringToBackend : String -> Backend Msg
stringToBackend string =
    Maybe.withDefault RamDict.backend <| Types.get string backends

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\x -> Sub.none)
        }

type alias Model =
    { authorization : Maybe Authorization
    , backendType: String
    , backend : Backend Msg
    , result : Maybe BackendResult
    , username : String
    , password : String
    , uploadType : UploadType
    , path : String
    , content : String
    , downloadResult : Maybe BackendResult
    }

emptyOperation : BackendOperation
emptyOperation =
    Types.Authorize (.state RamDict.backend) <| Authorization "" ""

init : ( Model, Cmd msg)
init =
    ( { authorization = Nothing
      , backendType = "RamDict"
      , backend = RamDict.backend
      , result = Just <| Err ( OtherBackendError "No operation initiated."
                             , emptyOperation)
      , username = ""
      , password = ""
      , uploadType = Page
      , path = "foo"
      , content = "something"
      , downloadResult = Nothing
      }
    , Cmd.none
    )

type Msg
    = UpdateUsername String
    | UpdatePassword String
    | UpdateUploadType String
    | UpdateBackend String
    | UpdatePath String
    | UpdateContent String
    | Authorize
    | Upload
    | Download
    | Receive BackendResult
    | ReceiveDownload BackendResult
          
stringToUploadType : String -> UploadType
stringToUploadType string =
    case string of
        "settings" -> Settings
        "template" -> Template
        "image" -> Image
        _ -> Page

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      UpdateUsername username ->
          ( { model | username = username }
          , Cmd.none
          )
      UpdatePassword password ->
          ( { model | password = password }
          , Cmd.none
          )
      UpdateUploadType uploadType ->
          ( { model | uploadType = stringToUploadType uploadType }
          , Cmd.none
          )
      UpdateBackend backendType ->
          ( { model | backend = stringToBackend backendType
            , authorization = Nothing
            }
          , Cmd.none
          )
      UpdatePath path ->
          ( { model | path = path }
          , Cmd.none
          )
      UpdateContent content ->
          ( { model | content = content }
          , Cmd.none
          )
      Authorize ->
          ( { model | result = Nothing }
            , authorize_ model.backend Receive model.username model.password
          )
      Upload ->
          case model.authorization of
              Nothing ->
                  ( { model
                        | result =
                            Just <| Err (AuthorizationError, emptyOperation)
                    }
                  , Cmd.none
                  )
              Just authorization ->
                  ( { model | result = Nothing }
                  , uploadFile model.backend Receive
                      authorization
                      model.uploadType
                      model.path
                      model.content
                  )
      Download ->
          ( { model | downloadResult = Nothing }
          , downloadFile model.backend ReceiveDownload
              model.uploadType
              model.path
          )
      Receive result ->
          let authorization =
                  case result of
                      Ok (Types.Authorize state auth) ->
                          Just auth
                      _ ->
                          model.authorization
          in
              ( { model
                    | result = Just result
                    , authorization = authorization
                    , backend = updateStateFromResult result model.backend
                }
              , Cmd.none
              )
      ReceiveDownload result ->
          ( { model
                | downloadResult = Just (log "ReceiveDownload" result)
                , backend = updateStateFromResult result model.backend
            }
          , Cmd.none
          )

br : Html msg
br =
    Html.br [][]

backendSelector : Model -> Html Msg
backendSelector model =
    select [ onInput UpdateBackend ]
    <| List.map (\(name, _) ->
                  option [ value name
                         , selected (name == model.backendType)
                         ]
                  [ text name ]
                )
                backends

view : Model -> Html Msg
view model =
    div [ style [("margin", "auto")] ]
        [ p [] [ text <| resultString model ]
        , p []
            [ text "backend: "
            , backendSelector model
            ]
        , p []
            [ text "username: "
            , input [ type_ "text"
                    , onInput UpdateUsername
                    , value model.username
                    ]
                []
            , br
            , text "password: "
            , input [ type_ "password"
                    , onInput UpdatePassword
                    , value model.password
                    ]
                []
            , text " "
            , button [ onClick Authorize ]
                [ text "Authorize" ]
            ]
        , p []
            [ text "uploadType: "
            , select [ onInput UpdateUploadType
                     ]
                <|
                let t = model.uploadType
                in
                    [ option [ value "page"
                             , selected (t == Page)
                             ]
                          [ text "Page" ]
                    , option [ value "image"
                             , selected (t == Image)
                             ]
                          [ text "Image" ]
                    , option [ value "template"
                             , selected (t == Template)
                             ]
                          [ text "Template" ]
                    , option [ value "settings"
                             , selected (t == Settings)
                             ]
                          [ text "Settings" ]
                    ]
            , br
            , text "path: "
            , input [ type_ "text"
                    , onInput UpdatePath
                    , value model.path
                    ]
                []
            , text " "
            , button [ onClick Download ]
                [ text "Download" ]
            , br
            , text "content: "
            , input [ type_ "text"
                    , onInput UpdateContent
                    , value model.content
                    ]
                []
            , text " "
            , button [ onClick Upload ]
                [ text "Upload" ]
            ]
        , renderDownloadResult model.downloadResult
        ]

renderDownloadResult : Maybe (BackendResult) -> Html Msg
renderDownloadResult maybeResult =
    case maybeResult of
        Nothing ->
            text ""
        Just result ->
            let (path, content) =
                    case result of
                        Err (err, operation) ->
                            ( Types.operationPath operation
                            , Types.backendErrorToString err operation
                            )
                        Ok operation ->
                            ( Types.operationPath operation
                            , case operation of
                                  Types.DownloadFile _ _ _ maybeContent ->
                                      case maybeContent of
                                          Nothing ->
                                              "<blank>"
                                          Just content ->
                                              content
                                  _ ->
                                      "<not DownloadFile operation>"
                            )
            in
                p []
                    [ text "path: "
                    , text path
                    , br
                    , text content
                    ]

resultString : Model -> String
resultString model =
    case model.result of
        Nothing -> "Awaiting backend response."
        Just res ->
            case res of
                Err (err, operation) ->
                    Types.backendErrorToString err operation
                Ok _ ->
                    "Success!"
