----------------------------------------------------------------------
--
-- ApachePost.elm
-- Xossbow Backend that posts using a Perl script: site/cgi/upload.cgi
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Backend.ApachePost exposing ( backend )

import Xossbow.Types exposing ( UploadType(..), Authorization
                              , BackendOperation(..), BackendWrapper, Backend
                              )

import Http
import Debug exposing ( log )
import Base64
import Task

backend : Backend msg
backend =
    { name = "ApachePost"
    , description = "Post with the Perl script in site/cgi/upload.cgi."
    , operator = operate
    }

operate : BackendWrapper msg -> BackendOperation -> Cmd msg
operate wrapper operation =
    case operation of
        DownloadFile uploadType path _ ->
            downloadFile operation wrapper uploadType path
        Authorize authorization ->
            authorize operation wrapper authorization
        UploadFile authorization uploadType path content ->
            uploadFile operation wrapper authorization uploadType path content
        DeleteFile authorization uploadType path ->
            deleteFile operation wrapper authorization uploadType path

uploadTypeToString : UploadType -> String
uploadTypeToString uploadType =
    case uploadType of
        Settings -> "settings"
        Page -> "page"
        Template -> "template"
        Image -> "image"

fetchUrl : String -> ((Result Http.Error String) -> msg) -> Cmd msg
fetchUrl url wrapper =
    Http.send wrapper <| httpGetString (log "Getting URL" url)

httpGetString : String -> Http.Request String
httpGetString url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Cache-control" "no-cache" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

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

downloadFile : BackendOperation -> BackendWrapper msg -> UploadType -> String -> Cmd msg
downloadFile operation wrapper uploadType path =
    let url = uploadPath uploadType path
        wrap = (\res ->
                    case res of
                        Err err ->
                            wrapper
                            <| Err (toString err, operation)
                        Ok string ->
                            wrapper
                            <| Ok (DownloadFile uploadType path <| Just string)
               )
    in
        fetchUrl url wrap

helloScript : String
helloScript =
    "cgi/hello.cgi"

uploadScript : String
uploadScript =
    "cgi/upload.cgi"

-- https://en.wikipedia.org/wiki/Basic_access_authentication#Client_side
authorizationHeader : Authorization -> Http.Header
authorizationHeader { username, password } =
    Http.header "Authorization"
        <| "Basic " ++ (Result.withDefault "" --default can't happen
                            <| Base64.encode (username ++ ":" ++ password)
                       )

httpGetWithAuthorization : String -> Authorization -> Http.Request String
httpGetWithAuthorization url authorization =
    Http.request
        { method = "GET"
        -- Web browsers will query themselves.
        -- If you include the header, then their query happens over and over.
        -- May want an option, though.
        , headers = []  --[ authorizationHeader authorization ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

httpWrapper : BackendOperation -> BackendWrapper msg -> Result Http.Error String -> msg
httpWrapper operation wrapper result =
    wrapper <|
        case result of
            Err err ->
                Err (toString err, operation)
            Ok ok ->
                if String.trim ok == "OK" then
                    Ok operation
                else
                    Err ("Bad return value: " ++ ok, operation)

authorize : BackendOperation -> BackendWrapper msg -> Authorization -> Cmd msg
authorize operation wrapper authorization =
    Http.send (httpWrapper operation wrapper)
        <| httpGetWithAuthorization
            (log "Authorizing with" helloScript) authorization

httpPost : String -> Authorization -> Http.Body -> Http.Request String
httpPost url authorization body =
    Http.request
        { method = "POST"
        , headers = []  --[ authorizationHeader authorization ]
        , url = url
        , body = body
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

uploadFile : BackendOperation -> BackendWrapper msg -> Authorization -> UploadType -> String -> String -> Cmd msg
uploadFile operation wrapper authorization uploadType path content =
    Http.send (httpWrapper operation wrapper)
        <| httpPost (log "Posting with" uploadScript) authorization
        <| Http.multipartBody
            [ Http.stringPart "type" (uploadTypeToString uploadType)
            , Http.stringPart "name" path
            , Http.stringPart "content" content
            ]

deleteFile : BackendOperation -> BackendWrapper msg -> Authorization -> UploadType -> String -> Cmd msg
deleteFile operation wrapper authorization uploadType path =
    Task.attempt (\_ -> wrapper
                      <| Err ("DeleteFile not yet implemented", operation)
                 )
        <| Task.succeed ()
