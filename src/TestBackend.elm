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
             , BackendOperation, BackendWrapper, Backend
             , operate
             )
import Xossbow.Backend.ApachePost as ApachePost

import Html exposing ( Html, Attribute
                     , div, p, text, input, select, option, button
                     )
import Html.Attributes as Attributes
    exposing ( type_, style, href, rows, cols, class, value, selected )
import Html.Events exposing ( onClick, onInput )

log = Debug.log

-- This will eventually be a list, with a selector
backend : Backend Msg
backend =
    ApachePost.backend

main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (\x -> Sub.none)
        }

type alias Model =
    { authorization : Maybe Authorization
    , result : Maybe (Result (String , BackendOperation) BackendOperation)
    , username : String
    , password : String
    , uploadType : UploadType
    , path : String
    , content : String
    }

emptyOperation : BackendOperation
emptyOperation =
    Types.Authorize <| Authorization "" ""

init : ( Model, Cmd msg)
init =
    ( { authorization = Nothing
      , result = Just <| Err ("No operation initiated.", emptyOperation)
      , username = ""
      , password = ""
      , uploadType = Page
      , path = "foo"
      , content = "something"
      }
    , Cmd.none
    )

type Msg
    = UpdateUsername String
    | UpdatePassword String
    | UpdateUploadType String
    | UpdatePath String
    | UpdateContent String
    | Authorize
    | Upload
    | Receive (Result (String, BackendOperation) BackendOperation)
          
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
            , operate backend Receive
                <| Types.Authorize
                <| Authorization model.username model.password
          )
      Upload ->
          case model.authorization of
              Nothing ->
                  ( { model
                        | result =
                            Just <| Err ("Not authorized.", emptyOperation)
                    }
                  , Cmd.none
                  )
              Just authorization ->
                  ( { model | result = Nothing }
                  , operate backend Receive
                      <| Types.UploadFile
                          authorization
                          model.uploadType
                          model.path
                          model.content
                  )
      Receive result ->
          let authorization =
                  case result of
                      Ok (Types.Authorize auth) ->
                          Just auth
                      _ ->
                          model.authorization
          in
              ( { model
                    | result = Just result
                    , authorization = authorization
                }
              , Cmd.none
              )

br : Html msg
br =
    Html.br [][]

view : Model -> Html Msg
view model =
    div [ style [("margin", "auto")] ]
        [ p [] [ text <| resultString model ]
        , p []
            [ text "username: "
            , input [ type_ "text"
                    , onInput UpdateUsername
                    , value model.username
                    ]
                []
            , br
            , text "password: "
            , input [ type_ "text"
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
        ]

resultString : Model -> String
resultString model =
    case model.result of
        Nothing -> "Awaiting backend response."
        Just res ->
            case res of
                Err (msg, _) ->
                    msg
                Ok _ ->
                    "Success!"
