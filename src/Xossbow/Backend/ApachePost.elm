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

backend : Backend msg
backend =
    { name = "ApachePost"
    , description = "Post with the Perl script in site/cgi/upload.cgi."
    , operator = operate
    }

operate : BackendWrapper msg -> BackendOperation -> Cmd msg
operate wrapper operation =
    case operation of
        Authorize authorization ->
            authorize operation wrapper authorization
        UploadFile authorization uploadType path content ->
            uploadFile operation wrapper authorization uploadType path content
        DeleteFile authorization uploadType path ->
            deleteFile operation wrapper authorization uploadType path

helloScript : String
helloScript =
    "cgi/hello.cgi"

uploadScript : String
uploadScript =
    "cgi/upload.cgi"

authorizationHeader : Authorization -> Http.Header
authorizationHeader { username, password } =
    Http.header "Authorization"
        <| "Basic " ++ (Result.withDefault "" --default can't happen
                            <| Base64.encode (username ++ ":" ++ password)
                       )

httpGet : Authorization -> String -> Http.Request String
httpGet authorization url =
    Http.request
        { method = "GET"
        , headers = [ authorizationHeader authorization ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }

authorize : BackendOperation -> BackendWrapper msg -> Authorization -> Cmd msg
authorize operation wrapper authorization =
    let wrap = (\res ->
                    wrapper <|
                        case res of
                            Err err ->
                                Err (toString err, operation)
                            Ok ok ->
                                if String.trim ok == "OK" then
                                    Ok operation
                                else
                                    Err ("Bad return value: " ++ ok, operation)
               )
    in
        Http.send wrap
            <| httpGet authorization (log "Authorizing with" helloScript)
    

uploadFile : BackendOperation -> BackendWrapper msg -> Authorization -> UploadType -> String -> String -> Cmd msg
uploadFile operation wrapper authorization uploadType path content =
    Cmd.none


deleteFile : BackendOperation -> BackendWrapper msg -> Authorization -> UploadType -> String -> Cmd msg
deleteFile operation wrapper authorization uploadType path =
    Cmd.none
