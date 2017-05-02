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

import Xossbow.Actions as Actions exposing ( ActionState, Action
                                           , makeActionState, nextAction
                                           )

import Dict exposing ( Dict )

type IndexingState msg
    = TheState (ActionState (IndexingRecord msg) msg)

type alias IndexingAction msg =
    Action (IndexingRecord msg) msg

type alias IndexingRecord msg =
    { perPage : Int
    , node : Node msg
    , backend : Backend msg
    , result : Maybe BackendResult
    }

type alias IndexingWrapper msg =
    IndexingState msg -> msg

type alias IndexingResult msg =
    Result (Backend msg, String) (Maybe (Backend msg), Cmd msg)

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

{- Here are the chains of actions that can be initiated here.
For each "added" (<tag>, <index>) pair:
If <index> is "" (which it should always be):
  1) Read tag/<tag>/index.txt
  2) If it does not exist, error. It should always exist at this point.
  3) Let <index> be the contents of tag/<tag>/index.txt
  4) Read tag/<tag>/<index>.txt
  5) If the number of pages in tag/<tag>/<index>.txt is less than perPage:
    a) Add <node>.path to the front of the pages.
    b) Write tag/<tag>/<index>.txt
    c) Update <node>.indices to map <tag> to <index>
    d) Write <node> to <node>.path
     Otherwise:
    e) <newIndex> = <index> + perPage
    f) Write tag/<tag>/<newIndex>.txt with <node>.path as its contents
    g) Read tag/<tag>/index.txt
    h) Replace <index> with <newIndex> and write tag/<tag>/index.txt
    i) Write tag/<tag>/<index>.txt with "next" set to <newIndex>
    j) Update <node>.indices to map <tag> to <newIndex>
    k) Write <node> to <node>.path

For each "removed" (<tag>, <index>) pair:
  1) Read tag/<tag>/<index>.txt
  2) If it exists:
    a) Remove <node>.path and write tag/<tag>/<index>.txt
    b) Do something smart if <index>.txt now contains no nodes.
       i) Update index.txt to point to the "previous" node,
          if it currently points at <index>.
       ii) Splice <index> out of the previous/next chain.
       
-}
updateIndices : Backend msg -> IndexingWrapper msg -> Int -> Node msg -> Dict String String -> Dict String String -> IndexingResult msg
updateIndices backend wrapper perPage node added removed =
    let record = { perPage = perPage
                 , node = node
                 , backend = backend
                 , result = Nothing
                 }
        actions = List.append (addedActions added) (removedActions removed)
        state = TheState <| makeActionState record actions
    in
        continueIndexing backend wrapper state

getBackend : ActionState (IndexingRecord msg) msg -> Backend msg
getBackend actions =
    let record = Actions.getState actions
    in
        record.backend

{-| Continues indexing via the state in the wrapper passed to `index`
or `continueIndexing`.
-}
continueIndexing : Backend msg -> IndexingWrapper msg -> IndexingState msg -> IndexingResult msg
continueIndexing backend wrapper (TheState state) =
    case nextAction state of
        Err msg ->
            Err (getBackend state, msg)
        Ok cmd ->
            if cmd /= Cmd.none then
                Ok (Nothing, cmd)
            else
                Ok (Just <| getBackend state, cmd)                

-- TODO
addedActions : Dict String String -> List (IndexingAction msg)
addedActions added =
    []

-- TODO
removedActions : Dict String String -> List (IndexingAction msg)
removedActions removed =
    []

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
