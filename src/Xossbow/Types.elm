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
                              , Plist, get
                              , UploadType(..), Authorization
                              , BackendOperation(..), BackendWrapper, Backend
                              , operate
                              )

import HtmlTemplate.Types exposing ( Atom(..) )

import Time exposing ( Time )

nodeVersion : Int
nodeVersion =
    1

type alias Node msg =
    { version : Int
    , comment : String
    , pageTemplate : String
    , nodeTemplate : String
    , title : String
    , path : String
    , author : String
    , time : Time
    , contentType : ContentType
    , rawContent : String
    , content : Atom msg
    , plist : Plist
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

type UploadType
    = Settings
    | Page
    | Template
    | Image

type alias Authorization =
    { username : String
    , password : String
    }

-- UploadFile Authorization UploadType path contents
-- DeleteFile Authorization UploadType path
type BackendOperation
    = Authorize Authorization
    | UploadFile Authorization UploadType String String
    | DeleteFile Authorization UploadType String

type alias BackendWrapper msg =
    Result (String, BackendOperation) BackendOperation -> msg

type alias Backend msg =
    { name : String
    , description : String
    , operator : BackendWrapper msg -> BackendOperation -> Cmd msg
    }

operate : Backend msg -> BackendWrapper msg -> BackendOperation -> Cmd msg
operate backend wrapper operation =
    let operator = backend.operator
    in
        operator wrapper operation
