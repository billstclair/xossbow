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

module Xossbow.Indexing exposing ( IndexingState, IndexingWrapper
                                 , index, continueIndexing
                                 )

import Xossbow.Types as Types exposing ( Node, Plist, UploadType(..)
                                       , Backend, BackendWrapper
                                       , get, downloadFile, uploadFile
                                       )

type IndexingState msg
    = IndexingDone

type alias IndexingWrapper msg =
    IndexingState -> msg

{-| Call when a node is created or changed.

Initiates the updates necessary to index a new or changed file.
Call after writing the file.
-}
index : Backend msg -> IndexingWrapper msg -> Maybe (Node msg) -> Node msg -> Cmd msg
index backend wrapper oldNode newNode =
    Cmd.none

{-| Continues indexing via the state in the wrapper passed to `index`
or `continueIndexing`.
-}
continueIndexing : Backend msg -> IndexingWrapper msg -> IndexingState msg -> Cmd msg
continueIndexing backend wrapper state =
    Cmd.none
