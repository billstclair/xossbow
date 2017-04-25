----------------------------------------------------------------------
--
-- RamDict.elm
-- Xossbow Backend that saves writes in an in-memory Dict.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Backend.RamDict exposing ( backend )

import Xossbow.Types exposing ( State(..), UploadType(..), Authorization
                              , BackendOperation(..)
                              , BackendError(..), BackendWrapper, Backend
                              , uploadPath, stateDict
                              )

import Xossbow.Backend.ApachePost as ApachePost

import Http
import Debug exposing ( log )
import Base64
import Task
import Dict exposing ( Dict )

apacheBackend : Backend msg
apacheBackend =
    ApachePost.backend

backend : Backend msg
backend =
    { name = "RamDict"
    , description = "For testing. Saves writes in an in-memory Dict."
    , operator = operate
    , state = DictState Dict.empty
    }

operate : BackendWrapper msg -> BackendOperation -> Cmd msg
operate wrapper operation =
    case operation of
        DownloadFile state uploadType path _ ->
            downloadFile state wrapper operation uploadType path
        Authorize _ authorization ->
            authorize wrapper operation authorization
        UploadFile state authorization uploadType path content ->
            uploadFile state wrapper operation authorization uploadType path content
        DeleteFile state authorization uploadType path ->
            deleteFile state wrapper operation authorization uploadType path

downloadFile : State -> BackendWrapper msg -> BackendOperation -> UploadType -> String -> Cmd msg
downloadFile state wrapper operation uploadType path =
    let url = uploadPath uploadType path
    in
        case Dict.get url (stateDict state) of
            Nothing ->
                apacheBackend.operator wrapper operation
            Just contents ->
                let task = Task.succeed
                           <| Ok
                           <| DownloadFile state uploadType path (Just contents)
                in
                    Task.perform wrapper <| task

knownUsername : String
knownUsername =
    "Xossbow"

knownPassword : String
knownPassword =
    "Xossbow"

authorize : BackendWrapper msg -> BackendOperation -> Authorization -> Cmd msg
authorize wrapper operation authorization =
    let { username, password } = authorization
        succeed = (username == knownUsername)
                  && (password == knownPassword)
        task = if succeed then
                   Task.succeed <| operation
               else
                   Task.fail <| (OtherBackendError "Bad username or password."
                                , operation
                                )
    in
        Task.attempt wrapper task                  

uploadFile : State -> BackendWrapper msg -> BackendOperation -> Authorization -> UploadType -> String -> String -> Cmd msg
uploadFile state wrapper operation authorization uploadType path content =
    let dict = Dict.insert path content <| stateDict state
        task = Task.succeed
               <| UploadFile
                   (DictState dict) authorization uploadType path content
    in
        Task.attempt wrapper task

deleteFile : State -> BackendWrapper msg -> BackendOperation -> Authorization -> UploadType -> String -> Cmd msg
deleteFile state wrapper operation authorization uploadType path =
    let dict = Dict.remove path <| stateDict state
        task = Task.succeed
               <| DeleteFile
                   (DictState dict) authorization uploadType path
    in
        Task.attempt wrapper task
