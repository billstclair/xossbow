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

import Xossbow.Types exposing ( State(..), UploadType(..), Authorization
                              , BackendOperation(..)
                              , BackendError(..), BackendWrapper, Backend
                              , uploadTypeToString, settingsPath, uploadPath
                              )

import Http
import Debug exposing ( log )
import Base64
import Task

state : State
state =
    NullState

backend : Backend msg
backend =
    { name = "ApachePost"
    , description = "Post with the Perl script in site/cgi/upload.cgi."
    , operator = operate
    , state = state
    }

operate : BackendWrapper msg -> BackendOperation -> Cmd msg
operate wrapper operation =
    case operation of
        DownloadFile _ uploadType path _ ->
            downloadFile wrapper operation uploadType path
        Authorize _ authorization ->
            authorize wrapper operation authorization
        UploadFile _ authorization uploadType path content ->
            uploadFile wrapper operation authorization uploadType path content
        DeleteFile _ authorization uploadType path ->
            deleteFile wrapper operation authorization uploadType path

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

toBackendError : Http.Error -> BackendError
toBackendError error =
    case error of
        Http.BadStatus response ->
            let code = response.status.code
            in
                if code == 401 then
                    AuthorizationError
                else if code == 404 then
                    NotFoundError
                else
                    OtherBackendError <| toString error
        _ ->
            OtherBackendError <| toString error

downloadFile : BackendWrapper msg -> BackendOperation -> UploadType -> String -> Cmd msg
downloadFile wrapper operation uploadType path =
    let url = uploadPath uploadType path
        wrap = (\res ->
                    case res of
                        Err err ->
                            wrapper
                            <| Err (toBackendError err, operation)
                        Ok string ->
                            wrapper
                            <| Ok (DownloadFile state uploadType path <| Just string)
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
                Err (toBackendError err, operation)
            Ok ok ->
                if String.trim ok == "OK" then
                    Ok operation
                else
                    Err (OtherBackendError <| "Backend error: " ++ ok, operation)

authorize : BackendWrapper msg -> BackendOperation -> Authorization -> Cmd msg
authorize wrapper operation authorization =
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

uploadFile : BackendWrapper msg -> BackendOperation -> Authorization -> UploadType -> String -> String -> Cmd msg
uploadFile wrapper operation authorization uploadType path content =
    Http.send (httpWrapper operation wrapper)
        <| httpPost (log "Posting with" uploadScript) authorization
        <| Http.multipartBody
            [ Http.stringPart "type" (uploadTypeToString uploadType)
            , Http.stringPart "name" path
            , Http.stringPart "content" content
            ]

deleteFile : BackendWrapper msg -> BackendOperation -> Authorization -> UploadType -> String -> Cmd msg
deleteFile wrapper operation authorization uploadType path =
    Task.attempt (\_ -> wrapper
                      <| Err (OtherBackendError "DeleteFile not yet implemented"
                             , operation
                             )
                 )
        <| Task.succeed ()
