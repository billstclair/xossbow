----------------------------------------------------------------------
--
-- Indexing.elm
-- Page indexing. See Indexing.md.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Indexing exposing ( IndexingState(..), IndexingWrapper
                                 , IndexingResult
                                 , index, continueIndexing
                                 )

import Xossbow.Types as Types exposing ( Node, Plist, UploadType(..)
                                       , Backend, BackendOperation(..)
                                       , BackendWrapper, BackendResult
                                       , BackendError(..)
                                       , get, downloadFile, uploadFile
                                       )

import Dict exposing ( Dict )

type IndexingState msg
    = TheState (IndexingRecord msg)

type Awaiting msg
  = AwaitingNothing
  | AwaitingAdd (String, String) (Node msg)
  | AwaitingRemove (String, String) (Node msg)
  | AwaitingWrite (Node msg) (Awaiting msg)
  | AwaitingIndex String (Awaiting msg)


type alias IndexingRecord msg =
    { perPage : Int
    , node : Node msg
    , result : Maybe BackendResult
    , awaiting : Awaiting msg
    , adds : List (String, String)
    , removes : List (String, String)
    }

type alias IndexingWrapper msg =
    IndexingState msg -> msg

type alias IndexingResult msg =
    Result String (Cmd msg)

{-| Call when a node is created or changed.

Initiates the updates necessary to index a new or changed file.
Call after writing the file.
-}
index : Backend msg -> IndexingWrapper msg -> Int -> Maybe (Node msg) -> Node msg -> IndexingResult msg
index backend wrapper perPage oldNode newNode =
    case oldNode of
        Nothing ->
            let added = newNode.indices
                removed = Dict.empty
            in
                updateIndices backend wrapper perPage newNode added removed
        Just old ->
            let added = newNode.indices --always ensure membership
                removed = Dict.diff old.indices newNode.indices
            in
                updateIndices backend wrapper perPage newNode added removed

updateIndices : Backend msg -> IndexingWrapper msg -> Int -> Node msg -> Dict String String -> Dict String String -> IndexingResult msg
updateIndices backend wrapper perPage node added removed =
    TheState { perPage = perPage
             , node = node
             , result = Nothing
             , awaiting = AwaitingNothing 
             , adds = Dict.toList added
             , removes = Dict.toList removed
             }
        |> continueIndexing backend wrapper

{-| Continues indexing via the state in the wrapper passed to `index`
or `continueIndexing`.
-}
continueIndexing : Backend msg -> IndexingWrapper msg -> IndexingState msg -> IndexingResult msg
continueIndexing backend wrapper (TheState state) =
    case processResult state.result state.awaiting state of
        Err msg ->
            Err msg
        Ok (state2, cmd) ->
            if cmd /= Cmd.none then
                Ok cmd
            else
                case state.adds of
                    pair :: rest ->
                        Ok <|
                        readIndex backend wrapper pair
                            { state2
                                | adds = rest
                                , awaiting = AwaitingAdd pair state2.node}
                    [] ->
                        case state.removes of
                            pair :: rest ->
                                Ok <|
                                readIndex backend wrapper pair
                                    { state2
                                        | removes = rest
                                        , awaiting = AwaitingRemove pair state2.node}
                            [] ->
                                Ok Cmd.none

tagsFile : String
tagsFile =
    "tag/index.txt"

tagDir : String -> String
tagDir tag =
    "tag/" ++ tag ++ "/"

tagIndexFile : String -> String
tagIndexFile tag =
    (tagDir tag) ++ "index.txt"

tagFile : String -> String -> String
tagFile tag name =
    (tagDir tag) ++ name ++ ".txt"

-- TODO: update and write out read result.
-- Process errors on write result
-- Eventually: do something reasonable about errors
-- Eventually: send along a hash of the old string, for collision detection.
processResult : Maybe BackendResult -> Awaiting msg -> IndexingRecord msg -> Result String (IndexingRecord msg, Cmd msg)
processResult result awaiting state =
    case result of
        Nothing ->
            Ok (state, Cmd.none)
        Just res ->
            case res of
                Err (NotFoundError, operation) ->
                    Err <| "Not found: " ++ (toString operation)
                Err err ->
                    Err <| toString err
                Ok operation ->
                    case awaiting of
                        AwaitingWrite _ _ ->
                            Ok ( { state | awaiting = AwaitingNothing }
                               , Cmd.none
                               )
                        _ ->
                            handleDownload operation awaiting state

handleDownload : BackendOperation -> Awaiting msg -> IndexingRecord msg -> Result String (IndexingRecord msg, Cmd msg)
handleDownload operation awaiting state =
    case operation of
        DownloadFile backendState _ _ contents ->
            Ok (state, Cmd.none)
        _ ->
            Ok (state, Cmd.none)                            

wrapBackendResult : BackendResult -> IndexingWrapper msg -> IndexingRecord msg -> msg
wrapBackendResult result indexingWrapper state =
  indexingWrapper <| TheState { state | result = Just result }

readIndex : Backend msg -> IndexingWrapper msg -> (String, String) -> IndexingRecord msg -> Cmd msg
readIndex backend wrapper pair state =
  let wrap = (\state res -> wrapBackendResult res wrapper state)
      (tag, name) = pair
  in
      if name == "" then
          let path = tagIndexFile tag
              state2 =
                  { state | awaiting = AwaitingIndex tag state.awaiting }
          in
              downloadFile backend (wrap state2) Page path
      else
          let path = tagFile tag name
          in
              downloadFile backend (wrap state) Page path
