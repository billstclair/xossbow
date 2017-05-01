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

module Xossbow.Actions exposing ( ActionState, Action
                                , makeActionState, nextAction
                                , getState, setState
                                , pushAction, discardAction
                                )

type ActionState state msg =
    TheActionState (StateRecord state msg)

type alias StateRecord state msg =
    { state : state
    , actions : List (Action state msg)
    }

type alias Action state msg =
    state -> ActionState state msg -> Cmd msg

makeActionState : state -> List (Action state msg) -> ActionState state msg
makeActionState state actions =
    TheActionState { state = state
                   , actions = actions
                   }

nextAction : ActionState state msg -> Cmd msg
nextAction (TheActionState actionState) =
    case actionState.actions of
        [] ->
            Cmd.none
        first :: rest ->
            first actionState.state
                <| TheActionState { actionState | actions = rest }

getState : ActionState state msg -> state
getState (TheActionState actionState) =
    actionState.state

setState : state -> ActionState state msg -> ActionState state msg
setState state (TheActionState actionState) =
    TheActionState { actionState | state = state }

pushAction : Action state msg -> ActionState state msg -> ActionState state msg
pushAction action (TheActionState actionState) =
    TheActionState { actionState | actions = action :: actionState.actions }

discardAction : ActionState state msg -> ActionState state msg
discardAction (TheActionState actionState) =
    case actionState.actions of
        [] ->
            TheActionState actionState
        _ :: rest ->
            TheActionState { actionState | actions = rest }
