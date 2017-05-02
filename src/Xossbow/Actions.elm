----------------------------------------------------------------------
--
-- Actions.elm
-- Support for performing a sequence of actions.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Actions exposing ( ActionState, Action, ActionResult
                                , makeActionState, nextAction
                                , getState, setState, mapState
                                , isEmpty
                                , pushAction, appendActions, discardAction
                                )

type ActionState state msg =
    TheActionState (StateRecord state msg)

type alias StateRecord state msg =
    { state : state
    , actions : List (Action state msg)
    }

type alias ActionResult msg =
    Result String (Cmd msg)

type alias Action state msg =
    state -> ActionState state msg -> ActionResult msg

makeActionState : state -> List (Action state msg) -> ActionState state msg
makeActionState state actions =
    TheActionState { state = state
                   , actions = actions
                   }

nextAction : ActionState state msg -> ActionResult msg
nextAction (TheActionState actionState) =
    case actionState.actions of
        [] ->
            Ok Cmd.none
        first :: rest ->
            first actionState.state
                <| TheActionState { actionState | actions = rest }

getState : ActionState state msg -> state
getState (TheActionState actionState) =
    actionState.state

setState : state -> ActionState state msg -> ActionState state msg
setState state (TheActionState actionState) =
    TheActionState { actionState | state = state }

mapState : (state -> state) -> ActionState state msg -> ActionState state msg
mapState f (TheActionState actionState) =
    TheActionState { actionState | state = f actionState.state }

isEmpty : ActionState state msg -> Bool
isEmpty (TheActionState state) =
    state.actions == []

pushAction : Action state msg -> ActionState state msg -> ActionState state msg
pushAction action (TheActionState actionState) =
    TheActionState { actionState | actions = action :: actionState.actions }

appendActions : List (Action state msg) -> ActionState state msg -> ActionState state msg
appendActions actions (TheActionState actionState) =
    TheActionState
    { actionState | actions = List.append actions actionState.actions }

discardAction : ActionState state msg -> ActionState state msg
discardAction (TheActionState actionState) =
    case actionState.actions of
        [] ->
            TheActionState actionState
        _ :: rest ->
            TheActionState { actionState | actions = rest }
