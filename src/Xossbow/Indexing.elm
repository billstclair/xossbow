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
    Result String (Backend msg, Cmd msg)

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
    case processResult backend state.result state.awaiting state of
        Err msg ->
            Err msg
        Ok (state2, backend2, cmd) ->
            if cmd /= Cmd.none then
                Ok (backend2, cmd)
            else
                case state.adds of
                    pair :: rest ->
                        Ok ( backend2
                           , readIndex backend2 wrapper pair
                               { state2
                                   | adds = rest
                                   , awaiting = AwaitingAdd pair state2.node}
                           )
                    [] ->
                        case state.removes of
                            pair :: rest ->
                                Ok ( backend2
                                   , readIndex backend2 wrapper pair
                                       { state2
                                           | removes = rest
                                           , awaiting =
                                               AwaitingRemove pair state2.node}
                                   )
                            [] ->
                                Ok (backend2, Cmd.none)

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
processResult : Backend msg -> Maybe BackendResult -> Awaiting msg -> IndexingRecord msg -> Result String (IndexingRecord msg, Backend msg, Cmd msg)
processResult backend result awaiting state =
    case result of
        Nothing ->
            Ok (state, backend, Cmd.none)
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
                               , Types.updateState operation backend
                               , Cmd.none
                               )
                        _ ->
                            handleDownload backend operation awaiting state

handleDownload : Backend msg -> BackendOperation -> Awaiting msg -> IndexingRecord msg -> Result String (IndexingRecord msg, Backend msg, Cmd msg)
handleDownload backend operation awaiting state =
    let backend2 = Types.updateState operation backend
    in
        case operation of
            DownloadFile _ uploadType path contents ->
                Ok (state
                   , backend2
                   ,Cmd.none)
            _ ->
                Ok (state
                   , backend2
                   , Cmd.none)

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
