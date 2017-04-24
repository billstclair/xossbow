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
                                 , index, continueIndexing
                                 )

import Xossbow.Types as Types exposing ( Node, Plist, UploadType(..)
                                       , Backend, BackendWrapper, BackendResult
                                       , get, downloadFile, uploadFile
                                       )

import Set exposing ( Set )
import Dict exposing ( Dict )

type IndexingState msg
    = TheState (IndexingRecord msg)

type Awaiting msg
  = AwaitingNothing
  | AwaitingAdd (String, String) (Node msg)
  | AwaitingRemove (String, String) (Node msg)
  | AwaitingWrite (Node msg) (Awaiting msg)


type alias IndexingRecord msg =
    { node : Node msg
    , result : Maybe BackendResult
    , awaiting : Awaiting msg
    , adds : List (String, String)
    , removes : List (String, String)
    }

type alias IndexingWrapper msg =
    IndexingState msg -> msg

{-| Call when a node is created or changed.

Initiates the updates necessary to index a new or changed file.
Call after writing the file.
-}
index : Backend msg -> IndexingWrapper msg -> Maybe (Node msg) -> Node msg -> Cmd msg
index backend wrapper oldNode newNode =
    case oldNode of
        Nothing ->
            let added = newNode.indices
                removed = Dict.empty
            in
                updateIndices backend wrapper newNode added removed
        Just old ->
            let added = newNode.indices
                removed = Dict.diff old.indices newNode.indices
            in
                updateIndices backend wrapper newNode added removed

updateIndices : Backend msg -> IndexingWrapper msg -> Node msg -> Dict String String -> Dict String String -> Cmd msg
updateIndices backend wrapper node added removed =
    TheState { node = node
             , result = Nothing
             , awaiting = AwaitingNothing 
             , adds = Dict.toList added
             , removes = Dict.toList removed
             }
        |> continueIndexing backend wrapper

{-| Continues indexing via the state in the wrapper passed to `index`
or `continueIndexing`.
-}
continueIndexing : Backend msg -> IndexingWrapper msg -> IndexingState msg -> Cmd msg
continueIndexing backend wrapper (TheState state) =
    let (state2, cmd) = processResult state.result state.awaiting state
    in
        if cmd /= Cmd.none then
          cmd
        else
          case state.adds of
              pair :: rest ->
                readIndex backend wrapper pair
                  { state2
                    | adds = rest
                    , awaiting = AwaitingAdd pair state2.node}
              [] ->
                case state.removes of
                    pair :: rest ->
                      readIndex backend wrapper pair
                        { state2
                          | removes = rest
                          , awaiting = AwaitingRemove pair state2.node}
                    [] ->
                      Cmd.none

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
processResult : Maybe BackendResult -> Awaiting msg -> IndexingRecord msg -> (IndexingRecord msg, Cmd msg)
processResult result awaiting state =
    (state, Cmd.none)

wrapBackendResult : BackendResult -> IndexingWrapper msg -> IndexingRecord msg -> msg
wrapBackendResult result indexingWrapper state =
  indexingWrapper <| TheState { state | result = Just result }

readIndex : Backend msg -> IndexingWrapper msg -> (String, String) -> IndexingRecord msg -> Cmd msg
readIndex backend wrapper (tag, file) state =
  let wrap = (\res -> wrapBackendResult res wrapper state)
  in
      Types.downloadFile backend wrap Page <| indexPath tag

-- This is wrong. Need the node to point to its index file
indexPath : String -> String
indexPath string =
  string
