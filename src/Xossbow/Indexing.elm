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

type alias IndexingRecord msg =
    { node : Node msg
    , result : Maybe BackendResult
    , indices : Dict String (Node msg)
    , reads : List String
    , adds : List String
    , removes : List String
    }

type alias IndexingWrapper msg =
    IndexingState -> msg

{-| Call when a node is created or changed.

Initiates the updates necessary to index a new or changed file.
Call after writing the file.
-}
index : Backend msg -> IndexingWrapper msg -> Maybe (Node msg) -> Node msg -> Cmd msg
index backend wrapper oldNode newNode =
    case oldNode of
        Nothing ->
            let added = Set.fromList newNode.tags
                removed = Set.empty
            in
                updateIndices backend wrapper newNode added removed
        Just old ->
            let oldSet = Set.fromList old.tags
                newSet = Set.fromList newNode.tags
                added = Set.diff newSet oldSet
                removed = Set.diff oldSet newSet
            in
                updateIndices backend wrapper newNode added removed

updateIndices : Backend msg -> IndexingWrapper msg -> Node msg -> Set String -> Set String -> Cmd msg
updateIndices backend wrapper node added removed =
    TheState { node = node
             , result = Nothing
             , indices = Dict.empty
             , reads = Set.toList <| Set.union added removed
             , adds = Set.toList added
             , removes = Set.toList removed
             }
        |> continueIndexing backend wrapper

{-| Continues indexing via the state in the wrapper passed to `index`
or `continueIndexing`.
-}
continueIndexing : Backend msg -> IndexingWrapper msg -> IndexingState msg -> Cmd msg
continueIndexing backend wrapper (TheState state) =
    let state2 = processResult state.result state
    in
        case state.reads of
            tag :: rest ->
                readTag backend wrapper tag
                    { state2 | reads = rest }
            [] ->
                case state.adds of
                    tag :: rest ->
                       addTag backend wrapper tag
                           { state2 | adds = rest }
                    [] ->
                        case state.removes of
                            tag :: rest ->
                                removeTag backend wrapper tag
                                    { state2 | removes = rest }
                            [] ->
                                Cmd.none

-- TODO: add a read node to indices
-- Eventually: do something reasonable about errors
processResult : Maybe BackendResult -> IndexingRecord msg -> IndexingRecord msg
processResult result state =
    state

readTag : Backend msg -> IndexingWrapper msg -> String -> IndexingRecord msg -> Cmd msg
readTag backend wrapper tag state =
    Cmd.none

addTag : Backend msg -> IndexingWrapper msg -> String -> IndexingRecord msg -> Cmd msg
addTag backend wrapper tag state =
    Cmd.none

removeTag : Backend msg -> IndexingWrapper msg -> String -> IndexingRecord msg -> Cmd msg
removeTag backend wrapper tag state =
    Cmd.none
