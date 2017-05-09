----------------------------------------------------------------------
--
-- Indexing.elm
-- Page indexing. See ../../Indexing.md.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Indexing exposing ( IndexingState, IndexingWrapper
                                 , IndexingResult
                                 , createTag, index, continueIndexing
                                 , tagsFile, tagDir, tagIndexFile, tagFile
                                 )

import Xossbow.Types as Types exposing ( Node, Plist, UploadType(..)
                                       , Backend, BackendOperation(..)
                                       , BackendWrapper, BackendResult
                                       , BackendError(..), Authorization
                                       , get, downloadFile, uploadFile
                                       )

import Xossbow.Actions as Actions exposing ( ActionState, Action, ActionResult
                                           , makeActionState, nextAction
                                           )

import Xossbow.Parsers as Parsers exposing ( parseNode, parseNodeContent )

import HtmlTemplate.Types exposing ( Atom(..) )

import HtmlTemplate.EncodeDecode exposing ( customEncodeAtom )

import Dict exposing ( Dict )
import List.Extra as LE
import Task
import Debug exposing ( log )

type IndexingState msg
    = TheState (IndexingActionState msg)

type alias IndexingActionState msg =
    ActionState (IndexingRecord msg) msg

type alias IndexingAction msg =
    Action (IndexingRecord msg) msg

type alias IndexingRecord msg =
    { perPage : Int
    , node : Node msg
    , backend : Backend msg
    , wrapper : IndexingWrapper msg
    , authorization : Authorization
    , result : Maybe BackendResult
    }

type alias IndexingWrapper msg =
    IndexingState msg -> msg

type alias IndexingResult msg =
    Result (Backend msg, String) (Maybe (Backend msg), Cmd msg)

{-| Create a new tag, or update the description of an existing tag.

    createTag backend wrapper tag description
-}
createTag : Backend msg -> IndexingWrapper msg -> Authorization -> String -> String -> Cmd msg
createTag backend wrapper authorization tag description =
    let record = { perPage = 10
                 , node = Types.emptyNode
                 , backend = backend
                 , wrapper = wrapper
                 , authorization = authorization
                 , result = Nothing
                 }
        actions = [ readTagIndex tag
                  , updateTagIndex tag description
                  , readTagsIndex
                  , updateTagsIndex tag description
                  ]
        state = TheState <| makeActionState record actions
    in
        case continueIndexing state of
            Err _ ->
                Cmd.none
            Ok (_, cmd) ->
                cmd

emptyTagIndexNode : String -> String -> String -> Node msg
emptyTagIndexNode tag description index =
    let content = LookupPageAtom index
        node = Types.emptyNode
        path = tagIndexFile tag
    in
        { node
            | nodeTemplate = "tagIndex"
            , comment = description
            , title = "Index for tag: " ++ tag
            , author = "Xossbow"
            , contentType = Types.Json
            , rawContent = customEncodeAtom 0 content
            , content = content
            , path = path
            , plist = [ ("tag", tag)
                      , ("permindex", index)
                      ]
        }

emptyTagNode : String -> String -> String -> Node msg
emptyTagNode tag description index =
    let content = ListAtom []
        node = Types.emptyNode
        path = tagFile tag index
    in
        { node
            | nodeTemplate = "index"
            , comment = description
            , title = "Index page for tag: " ++ tag
            , author = "Xossbow"
            , contentType = Types.Json
            , rawContent = customEncodeAtom 0 content
            , content = content
            , path = path
            , plist = [ ("tag", tag)
                      , ("permindex", index)
                      ]
        }

-- Expects to have just returned from `readTagIndex` with
-- the contents of `tag/<tag>/index.txt`, or a missing file error.
-- Updates the `description`, or creates a new file.
updateTagIndex : String -> String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
updateTagIndex tag description record actionState =
    let index = toString record.perPage
        processor = (\error state maybeNode ->
                         let node = case maybeNode of
                                        Just n ->
                                            { n | comment = description }
                                        Nothing ->
                                            emptyTagIndexNode tag description index
                             actions = case maybeNode of
                                           Just _ ->
                                               [ writeNode node ]
                                           Nothing ->
                                               let n = emptyTagNode
                                                       tag description index
                                               in
                                                   [ writeNode n
                                                   , writeNode node
                                                   ]
                         in
                             nextAction
                                 <| Actions.appendActions actions actionState
                    )
    in    
        processPossiblyMissingPageNode
        "updateTagIndex" True record actionState processor 

-- Check for error on `updateTagIndex` write.
-- Initiate read of `tag/index.txt`
readTagsIndex : IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
readTagsIndex record actionState =
    let processor = (\error state path _ ->
                         let as2 = updateBackendState record state actionState
                             cmd = downloadFile
                                   (getBackend as2)
                                   (wrapBackendResult record.wrapper as2)
                                   Page
                                   tagsFile
                         in
                             Ok cmd
                    )
    in
        processPossiblyMissingPageContents
            "readTagsIndex" False record actionState processor

emptyTagsIndexNode : String -> String -> Node msg
emptyTagsIndexNode tag description =
    let content = PListAtom [ (tag, StringAtom description) ]
        node = Types.emptyNode
        path = tagsFile
    in
        { node
            | nodeTemplate = "tagsIndex"
            , comment = "Tags"
            , title = "Tags Index"
            , author = "Xossbow"
            , contentType = Types.Json
            , rawContent = customEncodeAtom 0 content
            , content = content
            , path = path
        }

-- Expects to have just returned from `readTagsIndex` with
-- the contents of `tag/index.txt`, or a missing file error.
-- Updates the `description` for `tag`, or creates a new file.
updateTagsIndex : String -> String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
updateTagsIndex tag description record actionState =
    let processor =
            (\error state maybeNode ->
                 let node = case maybeNode of
                                Just n ->
                                    case parseNodeContent n of
                                        Err err ->
                                            Err err
                                        Ok atom ->
                                            case atom of
                                                PListAtom plist ->
                                                    Ok
                                                    { n | content =
                                                          PListAtom
                                                          <| Types.set
                                                              tag
                                                              (StringAtom
                                                                   description)
                                                              plist
                                                    }
                                                _ ->
                                                    Err "Indexes node not a plist."
                                Nothing ->
                                    Ok <| emptyTagsIndexNode tag description
                         in
                             case node of
                                 Err err ->
                                     error err
                                 Ok n ->
                                     nextAction
                                     <| Actions.pushAction (writeNode n) actionState
                    )
    in    
        processPossiblyMissingPageNode
        "updateTagsIndex" True record actionState processor 

{-| Call when a node is created or its tags are changed.

Initiates the updates necessary to index a new or changed file.
Call after writing the file.

Use the returned value to update the `Backend` in your model, or to return the
`Cmd` from your `update` function.

When you get a message resulting from the `IndexingWrapper` arg, pass the
wrapped `IndexingState` to `continueIndexing`.
-}
index : Backend msg -> IndexingWrapper msg -> Authorization -> Int -> Maybe (Node msg) -> Node msg -> Cmd msg
index backend wrapper authorization perPage oldNode newNode =
    case oldNode of
        Nothing ->
            let added = newNode.indices
                removed = Dict.empty
            in
                updateIndices
                    backend wrapper authorization perPage newNode added removed
        Just old ->
            let added = newNode.indices --always ensure membership
                removed = Dict.diff old.indices newNode.indices
            in
                updateIndices
                    backend wrapper authorization perPage newNode added removed

updateIndices : Backend msg -> IndexingWrapper msg -> Authorization -> Int -> Node msg -> Dict String String -> Dict String String -> Cmd msg
updateIndices backend wrapper authorization perPage node added removed =
    let record = { perPage = perPage
                 , node = node
                 , backend = backend
                 , wrapper = wrapper
                 , authorization = authorization
                 , result = Just (Ok <| DownloadFile backend.state Page "" Nothing)
                 }
        actions = List.append (addedActions added) (removedActions removed)
        state = TheState <| makeActionState record actions
    in
        case continueIndexing state of
            Err _ ->
                Cmd.none
            Ok (_, cmd) ->
                cmd

getBackend : ActionState (IndexingRecord msg) msg -> Backend msg
getBackend actions =
    let record = Actions.getState actions
    in
        record.backend

updatedBackend : IndexingActionState msg -> Backend msg
updatedBackend actionState =
    let record = Actions.getState actionState
    in
        case record.result of
            Nothing ->
                record.backend
            Just result ->
                Types.updateStateFromResult result record.backend

updatedActionState : IndexingActionState msg -> IndexingActionState msg
updatedActionState actionState =
    let record = Actions.getState actionState
    in
        case record.result of
            Nothing ->
                actionState
            Just result ->
                let backend = Types.updateStateFromResult result record.backend
                    r2 = { record | backend = backend }
                in
                    Actions.setState r2 actionState

{-| Continues indexing via the state in the wrapper passed to `index`
or `continueIndexing`.
-}
continueIndexing : IndexingState msg -> IndexingResult msg
continueIndexing (TheState state) =
    case nextAction state of
        Err msg ->
            Err (getBackend state, msg)
        Ok cmd ->
            if cmd /= Cmd.none then
                Ok (Nothing, cmd)
            else if Actions.isEmpty state then
                let backend = updatedBackend state
                in
                    Ok (Just backend, Cmd.none)
            else
                continueIndexing (TheState state)

{- For each "added" (<tag>, <index>) pair:
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
-}
addedActions : Dict String String -> List (IndexingAction msg)
addedActions added =
    List.map addedTagActions (Dict.toList added)
        |> List.concat

addedTagActions : (String, String) -> List (IndexingAction msg)
addedTagActions (tag, index) =
    if index /= "" then
        []
    else
        [ readTagIndex tag
        , readTagPage tag
        , writeNodePathToTagPage tag
        -- The node gets updated by a new action pushed by writeNodePathToTagPage
        ]

wrapBackendResult : IndexingWrapper msg -> IndexingActionState msg -> BackendResult -> msg
wrapBackendResult wrapper actionState result =
    Actions.mapState (\record -> { record | result = Just result })
                     actionState
        |> TheState
        |> wrapper

-- Initiate a read of "tag/<tag>/index.txt"
readTagIndex : String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
readTagIndex tag record actionState =
    let path = tagIndexFile tag
        as2 = log "readTagIndex" <| updatedActionState actionState
        cmd = downloadFile
              record.backend
              (wrapBackendResult record.wrapper as2)
              Page
              path
    in
        Ok cmd

actionError : String -> String -> ActionResult msg
actionError function message =
    Err <| "Indexing." ++ function ++ ": " ++ message

updateBackendState : IndexingRecord msg -> Types.State -> IndexingActionState msg -> IndexingActionState msg
updateBackendState record state actionState =
    let backend = record.backend
    in
        Actions.setState
            { record
                | backend = { backend | state = state }
            }
            actionState

-- Return a successful result with the value of `makeCmd`,
-- updating the backend state inside of `actionState` with `state`.
actionCmd : IndexingRecord msg -> IndexingActionState msg -> Types.State -> (Backend msg -> BackendWrapper msg -> Cmd msg) -> ActionResult msg
actionCmd record actionState state makeCmd =
    let as2 = updateBackendState record state actionState
    in
        simpleActionCmd (Actions.getState as2) as2 makeCmd
                    
-- Return a successful result with the value of `makeCmd`,
simpleActionCmd : IndexingRecord msg -> IndexingActionState msg -> (Backend msg -> BackendWrapper msg -> Cmd msg) -> ActionResult msg
simpleActionCmd record actionState makeCmd =
    let cmd = makeCmd
              record.backend
              (wrapBackendResult record.wrapper actionState)
    in
        Ok cmd

-- A wrapper around `Types.downloadFile` that puts the `path` first, so
-- it can easily be closed over.
downloadPage : String -> Backend msg -> BackendWrapper msg -> Cmd msg
downloadPage path backend wrapper =
    downloadFile backend wrapper Page path

-- A wrapper around `Types.uploadFile` that puts the `authorization`, `path`,
-- and `content` first, so they can easily be closed over.
uploadPage : Authorization -> String -> String -> Backend msg -> BackendWrapper msg -> Cmd msg
uploadPage authorization path content backend wrapper =
    uploadFile backend wrapper authorization Page path content

-- error state node
type alias PageNodeProcessor msg =
    (String -> ActionResult msg) -> Types.State -> Node msg -> ActionResult msg

-- error state maybeNode
type alias MaybePageNodeProcessor msg =
    (String -> ActionResult msg) -> Types.State -> Maybe (Node msg) -> ActionResult msg

-- error state path maybeContents
type alias MaybePageContentsProcessor msg =
    (String -> ActionResult msg) -> Types.State -> String -> Maybe String -> ActionResult msg

-- Do the uninteresting part of processing the contents of a just-read
-- (or just-written) page.
-- Call `processor` to do the actual work.
processPossiblyMissingPageContents : String -> Bool -> IndexingRecord msg -> IndexingActionState msg -> MaybePageContentsProcessor msg -> ActionResult msg
processPossiblyMissingPageContents function allowNotFound record actionState processor =
    let error = actionError function
    in
        case record.result of
            Nothing ->
                error "Missing result"
            Just result ->
                case result of
                    Err (err, operation) ->
                        if err == NotFoundError && allowNotFound then
                            case operation of
                                DownloadFile state _ path _ ->
                                    processor error state path Nothing
                                _ ->
                                    error "NotFoundError on non-download"
                        else
                            error <| toString result
                    Ok operation ->
                        case operation of
                            DownloadFile state _ path (Just contents) ->
                                processor error state path <| Just contents
                            UploadFile state _ _ path contents ->
                                processor error state path <| Just contents
                            operation ->
                                error <| toString result

-- Do the uninteresting part of processing the Node from a just-read
-- (or just-written) page that may not exist.
-- Call `processor` to do the actual work.
processPossiblyMissingPageNode : String -> Bool -> IndexingRecord msg -> IndexingActionState msg -> MaybePageNodeProcessor msg -> ActionResult msg
processPossiblyMissingPageNode function allowNotFound record actionState processor =
    let doit = (\error state path maybeContents ->
                    case maybeContents of
                        Nothing ->
                            processor error state Nothing
                        Just contents ->
                            case parseNode contents of
                                Err err ->
                                    error
                                    <| "Error parsing node: " ++ (toString err)
                                Ok node ->
                                    processor error state
                                        <| Just { node | path = path }
               )
    in
        processPossiblyMissingPageContents
            function allowNotFound record actionState doit

-- Do the uninteresting part of processing the Node from a just-read
-- (or just-written) page.
-- Call `processor` to do the actual work.
processPageNode : String -> IndexingRecord msg -> IndexingActionState msg -> PageNodeProcessor msg -> ActionResult msg
processPageNode function record actionState processor =
    let p = (\error state node ->
                 case node of
                     Nothing ->
                         error "Page does not exist. Can't happen."
                     Just n ->
                         processor error state n
            )
    in
        processPossiblyMissingPageNode
            function False record actionState p

permindex : Node msg -> Maybe String
permindex node =
    Types.get "permindex" node.plist

-- Expects record.result to be the result of readTagIndex.
-- If successful, and the parsed page has a non-empty "permindex" property,
-- returns a command to read that page.
-- Otherwise, returns an error.
readTagPage : String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
readTagPage tag record actionState =
    let processor = (\error state node ->
                         case permindex node of
                             Nothing ->
                                 error "missing permindex property"
                             Just name ->
                                 actionCmd record actionState state
                                     <| downloadPage (tagFile tag name)
                    )
    in
        processPageNode "readTagPage" record actionState processor

-- Expects record.result to be the result of readTagPage
-- If successful, and the parsed page is a list of `LookupPageAtom`s,
-- will add record.node's path to the top, and write.
-- If the list is already of length record.perPage or greater,
-- and its `next` link is blank, will instead
-- adds `Action`s to the list to create a new index page, link the
-- current index to it via `next`, and update "tag/<tag>/index.html" to
-- point to the new page.
writeNodePathToTagPage : String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
writeNodePathToTagPage tag record actionState =
    let processor = (\error state node ->
                         case parseNodeContent node of
                             Err err ->
                                 error err
                             Ok atom ->
                                 case atom of
                                     ListAtom list ->
                                         let as2 = updateBackendState
                                                   record state actionState
                                             r2 = Actions.getState as2
                                         in
                                             writeNodePathToTagPageInternal
                                                 tag r2 as2 node list
                                     _ ->
                                         error "Index node not a list."
                    )
    in
        processPageNode "writeNodePathToTagPage" record actionState processor

stateCmd : IndexingWrapper msg -> IndexingActionState msg -> Cmd msg
stateCmd wrapper actionState =
    Task.perform wrapper (Task.succeed <| TheState actionState)

isLookupPageAtom : String -> Atom msg -> Bool
isLookupPageAtom path atom =
    case atom of
        LookupPageAtom s ->
            s == path
        _ ->
            False

writeNodePathToTagPageInternal : String -> IndexingRecord msg -> IndexingActionState msg -> Node msg -> List (Atom msg) -> ActionResult msg
writeNodePathToTagPageInternal tag record actionState indexNode list =
    let node = record.node
        path = node.path
        updateNode = (\_ ->
                          case permindex indexNode of
                              Nothing ->
                                  node --maybe this should error instead
                              Just index ->
                                  let indices = Dict.insert
                                                tag index node.indices
                                  in
                                      { node | indices = indices }
                     )
    in
        case LE.find (isLookupPageAtom path) list
        of
            Just _ ->
                let as2 = Actions.pushAction
                          (writeNode <| (log "writeNode" <| updateNode ()))
                          actionState
                in
                    nextAction as2
            Nothing ->
                if List.length list < record.perPage then
                    -- The new page fits in the current index
                    let in2 = { indexNode
                                    | content =
                                        ListAtom
                                        <| LookupPageAtom path :: list
                              }
                        as2 = Actions.appendActions
                              [ writeNode in2
                              , writeNode <| updateNode ()
                              ]
                              actionState
                    in
                        nextAction as2
                else
                    -- The new page does NOT fit in the current index.
                    -- Need to make a new index.
                    writeNewIndexPage tag record actionState indexNode

writeNewIndexPage : String -> IndexingRecord msg -> IndexingActionState msg -> Node msg -> ActionResult msg
writeNewIndexPage tag record actionState indexNode =
    let error = actionError "writeNewIndexPage"
    in
        case permindex indexNode of
            Nothing ->
                error "Missing permindex property"
            Just index ->
                case String.toInt index of
                    Err _ ->
                        error <| "Index not an integer: " ++ index
                    Ok idx ->
                        let newidx = toString <| idx + record.perPage
                            in2 = { indexNode
                                      | plist = Types.set
                                                "next" newidx indexNode.plist
                                  }
                            node = record.node
                            n2 = { node | indices = Dict.insert
                                                    tag newidx node.indices
                                 }
                            newin = { indexNode
                                        | plist = Types.set
                                                  "permindex" newidx
                                                  <| Types.set
                                                      "previous" index
                                                      indexNode.plist
                                        , path = tagFile tag newidx
                                        , content = ListAtom
                                                    [ LookupPageAtom node.path ]
                                    }
                            as2 = Actions.appendActions
                                  [ writeNode newin
                                  , writeNode in2
                                  , readTagIndex tag --could save earlier read
                                  , writeTagIndex tag newidx
                                  , writeNode n2
                                  ]
                                  actionState
                        in
                            nextAction as2

-- Expects record.result to be the result of `readTagIndex`,
-- the contents of `tag/<tag>/index.txt`.
-- Changes it to point to `newidx` and initiates write.
writeTagIndex : String -> String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
writeTagIndex tag newidx record actionState =
    let processor = (\error state node ->
                         let path = tagIndexFile tag
                             n = { node | content = LookupPageAtom path }
                         in
                             nextAction
                                 <| Actions.pushAction (writeNode n) actionState
                    )
    in
        processPageNode "writeTagIndex" record actionState processor

-- Write `node` to the server
-- Assumes the `content` is up-to-date, but `rawContent` may not be.
-- If there's a result, uses it to update the backend
writeNode : Node msg -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
writeNode node record actionState =
    let n = Parsers.setNodeContent node.content node
        content = Parsers.encodeNode n
        as2 = updatedActionState actionState
        r2 = Actions.getState as2
    in
        simpleActionCmd r2 as2
            <| uploadPage r2.authorization n.path content

{- For each "removed" (<tag>, <index>) pair:
  1) Read tag/<tag>/<index>.txt
  2) If it exists:
    a) Remove <node>.path and write tag/<tag>/<index>.txt
    b) Do something smart if <index>.txt now contains no nodes.
       i) Update index.txt to point to the "previous" node,
          if it currently points at <index>.
       ii) Splice <index> out of the previous/next chain.
       
-}
removedActions : Dict String String -> List (IndexingAction msg)
removedActions removed =
    List.map removedTagActions (Dict.toList removed)
        |> List.concat

removedTagActions : (String, String) -> List (IndexingAction msg)
removedTagActions (tag, index) =
    if index == "" then
        []
    else
        [ readTagPageFromIndex tag index
        , removeNodePathFromTagPage tag
        ]

-- Initiate a read of a tag file.
-- Error checks the preceding command and updates the backend state from it.
readTagPageFromIndex : String -> String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
readTagPageFromIndex tag index record actionState =
    let path = tagFile tag index
        processor = (\_ state _ _ ->
                         actionCmd record actionState state
                             <| downloadPage path
                    )
    in
        processPossiblyMissingPageContents
            "readTagPageFromIndex" True record actionState processor
        
-- Expects `record.result` to be the result of `readTagPageFromIndex`.
-- Removes the path for `record.node` from that page, and
-- initiates a write.
-- TODO: Something smart when deleting leaves an empty page.
removeNodePathFromTagPage : String -> IndexingRecord msg -> IndexingActionState msg -> ActionResult msg
removeNodePathFromTagPage tag record actionState =
    let node = record.node
        path = node.path
        updateState = (\state -> updateBackendState record state actionState)
        updateContent = (\atom list node ->
                             let content = ListAtom <| LE.remove atom list
                                 n2 = { node | content = content }
                             in
                                 nextAction
                                 <| Actions.pushAction (writeNode n2) actionState
                        )
        processor = (\error state node ->
                         case parseNodeContent node of
                             Err err ->
                                 error
                                 <| "Error parsing node: " ++ (toString err)
                             Ok content ->
                                 case content of
                                     ListAtom list ->
                                         case LE.find
                                             (isLookupPageAtom path)
                                             list
                                         of
                                             Nothing ->
                                                 Ok <| stateCmd record.wrapper
                                                     <| updateState state
                                             Just atom ->
                                                 updateContent atom list node
                                     x ->
                                         error
                                         <| "Expecting ListAtom: " ++ (toString x)
                    )
    in
        processPageNode "removeNodePathFromTagPage" record actionState processor

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
