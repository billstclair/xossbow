----------------------------------------------------------------------
--
-- Types.elm
-- Xossbow shared types
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Types exposing ( Node, nodeVersion, emptyNode
                              , ContentType (..)
                              , Plist, get, set
                              , State(..)
                              , Backend
                              , UploadType(..), Authorization
                              , BackendOperation(..)
                              , BackendError(..), BackendResult, BackendWrapper
                              , stateDict, backendDict, operationDict
                              , updateState, updateStateFromResult
                              , operate, downloadFile
                              , authorize, authorize_
                              , uploadFile, deleteFile
                              , uploadTypeToString, settingsPath, uploadPath
                              )

import HtmlTemplate.Types exposing ( Atom(..) )

import Time exposing ( Time )
import Dict exposing ( Dict )

nodeVersion : Int
nodeVersion =
    1

type alias Node msg =
    { version : Int             --nodeVersion
    , comment : String          --visible only to editor
    , pageTemplate : String
    , nodeTemplate : String
    , title : String
    , path : String
    , author : String
    , time : Time
    , indices : Dict String String -- tag -> index
    , contentType : ContentType
    , rawContent : String
    , content : Atom msg
    , plist : Plist             --on backend. All properties saved.
    }

emptyNode : Node msg
emptyNode =
    { version = nodeVersion
    , comment = ""
    , pageTemplate = "page"
    , nodeTemplate = "node"
    , title = "Untitled"
    , path = "nada"
    , author = "Unknown"
    , time = -433540800000 + (8 * 3600 * 1000)
    , indices = Dict.empty
    , contentType = Markdown
    , rawContent = "You were expecting maybe a treatise?"
    , content = ListAtom []
    , plist = []
    }

type ContentType
    = Json
    | Markdown
    | Text
    | Code

type alias Plist =
    List (String, String)

get : String -> Plist -> Maybe String
get key plist =
    case plist of
        [] ->
            Nothing
        (k, v) :: rest ->
            if key == k then
                Just v
            else
                get key rest

set : String -> String -> Plist -> Plist
set key value plist =
    (key, value) :: (List.filter (\(k,_) -> k /= key) plist)

type UploadType
    = Settings
    | Page
    | Template
    | Image

uploadTypeToString : UploadType -> String
uploadTypeToString uploadType =
    case uploadType of
        Settings -> "settings"
        Page -> "page"
        Template -> "template"
        Image -> "image"

settingsPath : String
settingsPath =
    "settings.json"

uploadPath : UploadType -> String -> String
uploadPath uploadType path =
    case uploadType of
        Settings ->
            settingsPath
        _ ->
            (uploadTypeToString uploadType) ++ "/" ++ path

type alias Authorization =
    { username : String
    , password : String
    }

type State
    = NullState
    | DictState (Dict String String)

type BackendOperation
    = DownloadFile State UploadType String (Maybe String)
    | Authorize State Authorization
    | UploadFile State Authorization UploadType String String
    | DeleteFile State Authorization UploadType String

type BackendError
    = AuthorizationError
    | NotFoundError
    | OtherBackendError String

-- Backends promise not to change their state if an operation
-- returns an error
type alias BackendResult =
    Result (BackendError, BackendOperation) BackendOperation

type alias BackendWrapper msg =
    BackendResult -> msg

type alias Backend msg =
    { name : String
    , description : String
    , operator : BackendWrapper msg -> BackendOperation -> Cmd msg
    , state : State
    }

stateDict : State -> Dict String String
stateDict state =
    case state of
        DictState dict ->
            dict
        _ ->
            Dict.empty

backendDict : Backend msg -> Dict String String
backendDict backend =
    stateDict backend.state

operationState : BackendOperation -> State
operationState operation =
    case operation of
        DownloadFile state _ _ _ ->
            state
        Authorize state _ ->
            state
        UploadFile state _ _ _ _ ->
            state
        DeleteFile state _ _ _ ->
            state

operationDict : BackendOperation -> Dict String String
operationDict operation =
    stateDict <| operationState operation

updateState : BackendOperation -> Backend msg -> Backend msg
updateState operation backend =
    { backend | state = operationState operation }

updateStateFromResult : BackendResult -> Backend msg -> Backend msg
updateStateFromResult result backend =
    updateState
        (case result of
             Err (_, operation) -> operation
             Ok operation -> operation
        )
        backend

operate : Backend msg -> BackendWrapper msg -> BackendOperation -> Cmd msg
operate backend wrapper operation =
    let operator = backend.operator
    in
        operator wrapper operation

downloadFile : Backend msg -> BackendWrapper msg -> UploadType -> String -> Cmd msg
downloadFile backend wrapper uploadType path =
    operate backend wrapper
        <| DownloadFile backend.state uploadType path Nothing

authorize : Backend msg -> BackendWrapper msg -> Authorization -> Cmd msg
authorize backend wrapper authorization =
    operate backend wrapper
        <| Authorize backend.state authorization

authorize_ : Backend msg -> BackendWrapper msg -> String -> String -> Cmd msg
authorize_ backend wrapper username password =
    authorize backend wrapper
        <| Authorization username password

uploadFile : Backend msg -> BackendWrapper msg -> Authorization -> UploadType -> String -> String -> Cmd msg
uploadFile backend wrapper authorization uploadType path content =
    operate backend wrapper
        <| UploadFile backend.state authorization uploadType path content

deleteFile : Backend msg -> BackendWrapper msg -> Authorization -> UploadType -> String -> Cmd msg
deleteFile backend wrapper authorization uploadType path =
    operate backend wrapper
        <| DeleteFile backend.state authorization uploadType path
