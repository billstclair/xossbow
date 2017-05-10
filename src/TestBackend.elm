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
             , BackendError(..), ContentType(..)
             , authorize_, uploadFile, downloadFile, updateStateFromResult
             , Node, emptyNode
             )
import Xossbow.Indexing exposing ( IndexingState, IndexingWrapper
                                 , IndexingResult
                                 , createTag, index, continueIndexing
                                 , tagsFile, tagIndexFile, tagFile
                                 )
import Xossbow.Backend.RamDict as RamDict
import Xossbow.Backend.ApachePost as ApachePost
import Xossbow.Parsers as Parsers

import HtmlTemplate.Types exposing ( Atom(..) )

import Html exposing ( Html, Attribute
                     , div, h2, p, text, input, select, option, button, pre
                     )
import Html.Attributes as Attributes
    exposing ( type_, style, href, rows, cols, class, value, selected )
import Html.Events exposing ( onClick, onInput )
import Dict
import String.Extra as SE

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
    , tags : String
    , downloadResult : Maybe BackendResult
    , tagsResult : Maybe String
    , tag: String
    , tagResult : Maybe String
    , tagIndex: String
    , tagIndexResult : Maybe String
    }

emptyOperation : BackendOperation
emptyOperation =
    Types.Authorize (.state RamDict.backend) <| Authorization "" ""

makeBackendResult : String -> Maybe BackendResult
makeBackendResult msg =
    Just
    <| Err ( OtherBackendError msg
           , emptyOperation
           )

defaultAuthorization : Authorization
defaultAuthorization =
    { username = "Xossbow"
    , password = "Xossbow"
    }

init : ( Model, Cmd msg)
init =
    ( { authorization = Just defaultAuthorization
      , backendType = "RamDict"
      , backend = RamDict.backend
      , result = makeBackendResult "No operation initiated."
      , username = ""
      , password = ""
      , uploadType = Page
      , path = "foo"
      , content = "something"
      , tags = "blog"
      , downloadResult = Nothing
      , tagsResult = Nothing
      , tag = "blog"
      , tagResult = Nothing
      , tagIndex = "10"
      , tagIndexResult = Nothing
      }
    , Cmd.none
    )

type Msg
    = UpdateUsername String
    | UpdatePassword String
    | UpdateUploadType String
    | UpdateBackend String
    | UpdatePath String
    | UpdateTags String
    | UpdateContent String
    | UpdateTag String
    | UpdateTagIndex String
    | Authorize
    | Upload
    | Download
    | ReadTags
    | ReadTag
    | CreateTag (Maybe String)
    | ReadTagIndex
    | Receive BackendResult
    | ReceiveUpload (Maybe (Node Msg)) (Node Msg) BackendResult
    | ReceiveDownloadForUpload Authorization BackendResult
    | ReceiveDownload BackendResult
    | ReceiveIndexing (IndexingState Msg)
    | ReceiveTags BackendResult
    | ReceiveTag BackendResult
    | ReceiveTagIndex BackendResult
          
stringToUploadType : String -> UploadType
stringToUploadType string =
    case string of
        "settings" -> Settings
        "template" -> Template
        "image" -> Image
        _ -> Page

downloadTags : Model -> Cmd Msg
downloadTags model =
    downloadFile model.backend ReceiveTags Page tagsFile

downloadTag : Model -> Cmd Msg
downloadTag model =
    downloadFile model.backend ReceiveTag Page
        <| tagIndexFile model.tag

downloadTagIndex : Model -> Cmd Msg
downloadTagIndex model =
    downloadFile model.backend ReceiveTagIndex Page
        <| tagFile model.tag model.tagIndex

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
            , authorization = if backendType == "RamDict" then
                                  Just defaultAuthorization
                              else
                                  Nothing
            }
          , Cmd.none
          )
      UpdatePath path ->
          ( { model | path = path }
          , Cmd.none
          )
      UpdateTags tags ->
          ( { model | tags = tags }
          , Cmd.none
          )
      UpdateContent content ->
          ( { model | content = content }
          , Cmd.none
          )
      UpdateTag tag ->
          ( { model | tag = tag }
          , Cmd.none
          )
      UpdateTagIndex index ->
          ( { model | tagIndex = index }
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
                  uploadContent authorization model
      Download ->
          ( { model | downloadResult = Nothing }
          , downloadFile model.backend ReceiveDownload
              model.uploadType
              model.path
          )
      ReadTags ->
          ( { model | tagsResult = Nothing }
          , downloadTags model
          )
      ReadTag ->
          ( { model | tagResult = Nothing }
          , downloadTag model
          )
      CreateTag maybeTag ->
          case model.authorization of
              Nothing ->
                  ( { model
                        | tagsResult = Just "Authorize to create tags index."
                    }
                  , Cmd.none
                  )
              Just authorization ->
                  let tag = case maybeTag of
                                Just tag -> tag
                                Nothing -> model.tag
                      description = SE.toTitleCase tag
                  in
                  ( model
                  , createTag model.backend ReceiveIndexing authorization
                      tag description
                  )
      ReadTagIndex ->
          ( { model | tagIndexResult = Nothing }
          , downloadTagIndex model
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
      ReceiveUpload oldNode node result ->
          let (m, _) = update (Receive result) model
          in
              case result of
                  Err _ -> (m, Cmd.none)
                  Ok _ ->
                      case m.tagsResult of
                          Nothing -> update Download m
                          Just _ ->
                              case m.authorization of
                                  Nothing -> (m, Cmd.none)
                                  Just authorization ->
                                      let cmd = index m.backend
                                                ReceiveIndexing
                                                authorization
                                                10
                                                oldNode
                                                node
                                      in
                                          if cmd == Cmd.none then
                                              update Download m
                                          else
                                              ( m, cmd)
      ReceiveDownloadForUpload authorization result ->
          let (m, _) = update (ReceiveDownload result) model
              oldNode = downloadedNode model
              node = contentNode oldNode model
          in
              ( m
              , uploadFile m.backend (ReceiveUpload oldNode node)
                  authorization
                  Page
                  m.path
                  <| Parsers.encodeNode node
              )              
      ReceiveDownload result ->
          ( { model
                | downloadResult = Just result
                , backend = updateStateFromResult result model.backend
            }
          , Cmd.none
          )
      ReceiveIndexing state ->
          case continueIndexing state of
              Err (backend, msg) ->
                  ( { model
                        | backend = backend
                        , result = makeBackendResult msg
                    }
                  , Cmd.none
                  )
              Ok (maybeBackend, cmd) ->
                  case maybeBackend of
                      Nothing ->
                          ( model, cmd )
                      Just backend ->
                          let m = { model | backend = backend }
                          in
                              case m.tagsResult of
                                  Nothing ->
                                      (m, Cmd.batch [cmd, downloadTags m])
                                  Just _ ->
                                      if cmd == Cmd.none then
                                          update Download m
                                      else
                                          (m, cmd)
      ReceiveTags result ->
          case result of
              Err (NotFoundError, _) ->
                  update (CreateTag <| Just "blog") model
              _ ->
                  receiveProperty result (\v -> { model | tagsResult = v })
      ReceiveTag result ->
          receiveProperty result (\v -> { model | tagResult = v })
      ReceiveTagIndex result ->
          receiveProperty result (\v -> { model | tagIndexResult = v })

uploadContent : Authorization -> Model -> (Model, Cmd Msg)
uploadContent authorization model =
    ( { model | result = Nothing }
    , case model.uploadType of
          Page ->
              downloadFile model.backend
                  (ReceiveDownloadForUpload authorization)
                  Page
                  model.path
          uploadType ->
              uploadFile model.backend Receive
                  authorization
                  uploadType
                  model.path
                  model.content
    )

downloadedNode : Model -> Maybe (Node Msg)
downloadedNode model =
    case model.downloadResult of
        Nothing ->
            Nothing
        Just result ->
            case result of
                Err _ ->
                    Nothing
                Ok operation ->
                    let (uploadType, path)
                            = Types.operationUploadTypeAndPath operation
                    in
                        if model.path == path
                            && model.uploadType == uploadType
                    then
                        case Parsers.parseNode
                            <| Types.downloadedContent operation
                        of
                            Err _ ->
                                Nothing
                            Ok node ->
                                Just node
                    else
                        Nothing
                            
contentNode : Maybe (Node msg) -> Model -> Node msg
contentNode maybeNode model =
    let tags = case model.tagsResult of
                   Nothing -> []
                   Just _ -> parseTags model.tags
        oldNode = case maybeNode of
                      Just node ->
                          node
                      Nothing ->
                          emptyNode
        oldIndices = oldNode.indices
        indices = Dict.fromList
                  <| List.map
                      (\tag ->
                           (tag, case Dict.get tag oldIndices of
                                     Nothing -> ""
                                     Just idx -> idx
                           )
                      )
                      tags
    in
      { oldNode
          | indices = indices
          , path = model.path
          , rawContent = model.content
          , contentType = Text
          , content = StringAtom model.content
      }

parseTags : String -> List String
parseTags string =
    String.split "," string
    |> List.map String.trim
    |> List.filter (\x -> x /= "")

receiveProperty : BackendResult -> (Maybe String -> Model) -> (Model, Cmd Msg)
receiveProperty result setter =
    case result of
        Err (err, operation) ->
            ( setter (Just <| Types.backendErrorToString err operation)
            , Cmd.none
            )
        Ok operation ->
            ( let m = setter (Just <| Types.downloadedContent operation)
              in
                  { m | backend = updateStateFromResult result m.backend }
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
    div [ style [ ("margin", "auto")
                , ("margin-top", "5em")
                , ("width", "40em")
                , ("padding", "2em")
                , ("border", "solid")
                ]
        ]
        [ h2 [ style [("text-align", "center")]]
              [ text "TestBackend" ]
        , p [] [ text <| resultString model ]
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
            , text "tags: "
            , input [ type_ "text"
                    , onInput UpdateTags
                    , value model.tags
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
        , renderDownloadResult model.downloadResult
        , p []
            [ button [ onClick ReadTags ]
                  [ text "Read Tags Index" ]
            , case model.tagsResult of
                  Nothing ->
                      text ""
                  Just string ->
                      pre []
                          [ text string ]
            ]
        , p []
            [ text "tag: "
            , input [ type_ "text"
                    , onInput UpdateTag
                    , value model.tag
                    ]
                []
            , text " "
            , button [ onClick ReadTag ]
                [ text "Read Tag" ]
            , text " "
            , button [ onClick <| CreateTag Nothing ]
                [ text "Create Tag" ]
            , case model.tagResult of
                  Nothing ->
                      br
                  Just string ->
                      pre []
                          [ text string ]
            , text "index: "
            , input [ type_ "text"
                    , onInput UpdateTagIndex
                    , value model.tagIndex
                    ]
                []
            , text " "
            , button [ onClick ReadTagIndex ]
                [ text "Read Tag Index" ]
            , case model.tagIndexResult of
                  Nothing ->
                      text ""
                  Just string ->
                      pre []
                          [ text string ]
            ]
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
                    , pre []
                        [ text content ]
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
