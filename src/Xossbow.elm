----------------------------------------------------------------------
--
-- Xossbow.elm
-- Blogging in Elm
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow exposing (..)

import Xossbow.Types as Types
    exposing ( Node, ContentType(..), emptyNode
             , Authorization, BackendWrapper, Backend
             , BackendOperation
             )
import Xossbow.Parsers exposing ( parseNode )
import Xossbow.Backend.ApachePost as ApachePost

import HtmlTemplate exposing ( makeLoaders, insertFunctions, insertMessages
                             , insertStringMessages
                             , addPageProcessors
                             , getExtra, setExtra, getDicts
                             , addOutstandingPagesAndTemplates
                             , loadPage, receiveCustomPage
                             , loadTemplate, receiveTemplate
                             , loadOutstandingPageOrTemplate
                             , maybeLoadOutstandingPageOrTemplate
                             , getPage, setPage, removePage
                             , getTemplate
                             , getAtom, setAtom, setAtoms, getDictsAtom
                             , clearPages
                             , render, eval, cantFuncall
                             )

import HtmlTemplate.Types exposing ( Loaders, Atom(..), Dicts(..) )

import HtmlTemplate.EncodeDecode
    exposing ( decodeAtom, encodeAtom, customEncodeAtom )

import HtmlTemplate.Markdown as Markdown

import HtmlTemplate.Utility as Utility exposing ( mergeStrings )

import HtmlTemplate.PlayDiv exposing ( PlayState, emptyPlayState
                                     , playDivFunction
                                     , Update, playStringUpdate, updatePlayState
                                     )

import Html exposing ( Html, Attribute
                     , div, p, text, a, textarea, pre
                     )
import Html.Attributes as Attributes exposing ( style, href, rows, cols, class )
import Html.Events exposing ( onClick, onInput )
import Http
import Date
import Time exposing ( Time, second )

import Navigation exposing ( Location )

log = Debug.log

main =
    Navigation.program
        Navigate
        { init = init
        , update = update
        , view = view
        , subscriptions = (\x -> Time.every second Tick)
        }

type alias Model =
    { loaders: Loaders Msg Extra
    , location : Location
    , page: Maybe String
    , pendingPage : Maybe String
    , playState : Maybe (PlayState Msg)
    , authorization : Maybe Authorization
    , time : Time
    , error : Maybe String
    }

templateDirs : List String
templateDirs =
    [ "default", "black", "red" ]

settingsFile : String
settingsFile =
    "settings"

settingsPageName : String
settingsPageName =
    "_settings"

indexTemplate : String
indexTemplate =
    "index"

pageTemplate : String
pageTemplate =
    "page"

nodeTemplate : String
nodeTemplate =
    "node"

initialTemplates : List String
initialTemplates =
    [ pageTemplate, indexTemplate, nodeTemplate ]

indexPage : String
indexPage =
    "index"

initialPages : List String
initialPages =
    [ settingsPageName ]

postTemplate : String
postTemplate =
    "post"

templateFileType : String
templateFileType =
    ".json"

templateFilename : String -> String
templateFilename name =
    name ++ templateFileType

pageFileType : String
pageFileType =
    ".txt"

pageFilename : String -> String
pageFilename name =
    name ++ pageFileType

gotoPageMessage : List (Atom Msg) -> d -> Msg
gotoPageMessage args _ =
    case args of
        [StringAtom page] ->
            GotoPage page
        _ ->
            SetError <| "Can't go to page: " ++ (toString args)

loginMessage : List (Atom Msg) -> d -> Msg
loginMessage args _ =
    case args of
        [StringAtom username, StringAtom password] ->
            Login username password
        _ ->
            SetError <| "You must specify a Username and Password."

setMessage : List (Atom Msg) -> d -> (String -> Msg)
setMessage args _ =
    case args of
        [StringAtom name] ->
            SetMsg name
        _ ->
            (\_ ->
                 SetError <| "Can't set value: " ++ (toString args)
            )

messages : List (String, List (Atom Msg) -> Dicts Msg -> Msg)
messages =
    [ ( "gotoPage", gotoPageMessage )
    , ( "login", loginMessage )
    ]

stringMessages : List (String, List (Atom Msg) -> Dicts Msg -> (String -> Msg))
stringMessages =
    [ ( "set", setMessage )
    ]

pageLinkFunction : List (Atom Msg) -> d -> Atom Msg
pageLinkFunction args _ =
    case normalizePageLinkArgs args of
        Just ( page, title ) ->
            HtmlAtom
            <| a [ href <| "#" ++ page
                 --, onClick <| GotoPage page
                 ]
                [ text title ]
        _ ->
            cantFuncall "pageLink" args

emailLinkFunction : List (Atom Msg) -> d -> Atom Msg
emailLinkFunction args _ =
    case args of
        [ StringAtom email ] ->
            HtmlAtom
            <| a [ href <| "mailto:" ++ email
                 ]
                [ text email ]
        _ ->
            cantFuncall "mailLink" args

normalizePageLinkArgs : List (Atom Msg) -> Maybe (String, String)
normalizePageLinkArgs atom =
    case atom of
        [ StringAtom page ] ->
            Just (page, page)
        [ StringAtom page, StringAtom title ] ->
            Just (page, title)
        _ ->
            Nothing

xossbowFunction : List (Atom Msg) -> d -> Atom Msg
xossbowFunction args _ =
    HtmlAtom
    <| a [ href <| "/" ]
        [ text "Xossbow" ]

emptyString : Atom msg
emptyString =
    StringAtom ""

getFunction : List (Atom Msg) -> Dicts Msg -> Atom Msg
getFunction args dicts =
    case args of
        [ StringAtom name ] ->
            Maybe.withDefault emptyString <|  getDictsAtom name dicts
        _ ->
            emptyString

setFunction : List (Atom Msg) -> Dicts Msg -> Atom Msg
setFunction args dicts =
    case args of
        [ StringAtom name, value ] ->
            Maybe.withDefault emptyString <|  getDictsAtom name dicts
        _ ->
            emptyString

functions : List (String, List (Atom Msg) -> Dicts Msg -> Atom Msg)
functions =
    [ ( "pageLink", pageLinkFunction )
    , ( "emailLink", emailLinkFunction )
    , ( "xossbow", xossbowFunction )
    , ( "get", getFunction )
    , ( "set", setFunction )
    ]

type alias Extra =
    { templateDir : String
    }

initialExtra : Extra
initialExtra =
    { templateDir = "default"
    }

pageProcessors : List (String, String -> Atom Msg -> Loaders Msg Extra -> Loaders Msg Extra)
pageProcessors =
    [ ( settingsPageName, installSettings )
    , ( "", add_page_Property )
    ]

initialLoaders : Loaders Msg Extra
initialLoaders =
    makeLoaders fetchTemplate fetchPage initialExtra
    |> setAtom "referer" (StringAtom indexPage)
    |> insertFunctions functions
    |> insertMessages messages
    |> insertStringMessages stringMessages
    |> addPageProcessors pageProcessors
    |> addOutstandingPagesAndTemplates initialPages initialTemplates

initialDicts : Dicts Msg
initialDicts =
    getDicts initialLoaders

---
--- init
---

init : Location -> ( Model, Cmd Msg)
init location =
    let model = { loaders = initialLoaders
                , location = location
                , page = Nothing
                , pendingPage = Nothing
                , playState = Nothing
                , authorization = Nothing
                , time = 0
                , error = Nothing
                }
    in
        ( model
        , loadOutstandingPageOrTemplate model.loaders
        )

-- Store just-loaded "settings" plist as an atom, and remove it as a page.
-- The pages reference it as "$settings.<property>".
installSettings : String -> Atom Msg -> Loaders Msg Extra -> Loaders Msg Extra
installSettings name settings loaders =
    setAtom settingsFile settings
        <| removePage name loaders

-- The value of most pages is a plist processed by the "node" template.
-- This adds the page's own name to its plist when it's loaded.
add_page_Property : String -> Atom Msg -> Loaders Msg Extra -> Loaders Msg Extra
add_page_Property name page loaders =
    case page of
        PListAtom plist ->
          setPage name (PListAtom <| ("page", StringAtom name) :: plist) loaders
        _ ->
          loaders

type Msg
    = TemplateFetchDone String (Loaders Msg Extra) (Result Http.Error String)
    | PageFetchDone String (Loaders Msg Extra) (Result Http.Error String)
    | GotoPage String
    | UpdatePlayState Update
    | SetError String
    | Navigate Location
    | Tick Time
    | SetMsg String String
    | Login String String
    | HandleLogin (Result (String, BackendOperation) BackendOperation)

fetchUrl : String -> ((Result Http.Error String) -> Msg) -> Cmd Msg
fetchUrl url wrapper =
    Http.send wrapper <| httpGetString (log "Getting URL" url)

httpGetString : String -> Http.Request String
httpGetString url =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Cache-control" "no-cache" ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }

templateDir : Loaders Msg Extra -> String
templateDir loaders =
    .templateDir <| getExtra loaders

fetchTemplate : String -> Loaders Msg Extra -> Cmd Msg
fetchTemplate name loaders =
    let filename = templateFilename name
        url = "template/" ++ (templateDir loaders) ++ "/" ++ filename
    in
        fetchUrl url <| TemplateFetchDone name loaders

fetchPage : String -> Loaders Msg Extra -> Cmd Msg
fetchPage name loaders =
    let url = if name == settingsPageName then
                  templateFilename settingsFile
              else
                  "page/" ++ (pageFilename name)
    in
        fetchUrl url <| PageFetchDone name loaders

---
--- update
---

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemplateFetchDone name loaders result ->
            templateFetchDone name loaders result model
        PageFetchDone name loaders result ->
            pageFetchDone name loaders result model
        GotoPage page ->
            gotoPage page model
        UpdatePlayState update ->
            updatePlayString update model
        SetError message ->
            ( { model | error = Just message }
            , Cmd.none
            )
        Tick time ->
            ( { model | time = time }
            , Cmd.none
            )
        Navigate location ->
            navigate location model
        SetMsg name value ->
            ( { model | loaders = setAtom name (StringAtom value) model.loaders }
            , Cmd.none
            )
        Login username password->
            login username password model
        HandleLogin result ->
            handleLogin result model

pageFromLocation : Location -> String
pageFromLocation location =
    let hash = location.hash
        sansSharp = if (String.left 1 hash) == "#" then
                        String.dropLeft 1 hash
                    else
                        hash
    in
        case String.split "." sansSharp of
            [] ->
                indexPage
            res :: _ ->
                if res == "" then
                    indexPage
                else
                    res

navigate : Location -> Model -> ( Model, Cmd Msg )
navigate location model =
    let newPage = pageFromLocation location
        mod = { model | location = location }
    in
        if case model.page of
               Nothing ->
                   True
               Just page ->
                   page /= newPage
        then
            gotoPage newPage mod
        else
            ( mod
            , Cmd.none
            )

gotoPage : String -> Model -> ( Model, Cmd Msg )
gotoPage page model =
    let m = { model | error = Nothing
            , pendingPage = Just page
            }
    in
        ( m
        , fetchPage page <| clearPages model.loaders
        )

templateFetchDone : String -> Loaders Msg Extra -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
templateFetchDone name loaders result model =
    case result of
        Err err ->
            ( { model
                  | error =
                      Just
                      <| "Error fetching template " ++ name ++ ": " ++ (toString err)
              }
            , Cmd.none
            )
        Ok json ->
            case receiveTemplate name json loaders of
                Err msg ->
                    ( { model
                          | loaders = loaders
                          , error =
                              Just
                              <| "While parsing template \"" ++
                                  name ++ "\": " ++ msg
                      }
                    , Cmd.none
                    )
                Ok loaders2 ->
                    continueLoading loaders2 model

continueLoading : Loaders Msg Extra -> Model -> ( Model, Cmd Msg )
continueLoading loaders model =
    case maybeLoadOutstandingPageOrTemplate loaders of
        Just cmd ->
            -- Do NOT update model.loaders yet, or the screen flashes
            ( model, cmd )
        Nothing ->
            let m = { model | loaders = loaders }
            in
                case m.pendingPage of
                    Nothing ->
                        -- This happens at startup
                        gotoPage (pageFromLocation m.location) m
                    Just page ->
                        ( { m |
                            page = Just page
                          , pendingPage = Nothing
                          , loaders = case m.page of
                                          Nothing -> loaders
                                          Just referer ->
                                              setAtom
                                                "referer"
                                                (StringAtom referer)
                                                loaders
                          }
                        , Cmd.none
                        )

parsePage : String -> String -> Result String (Atom Msg)
parsePage page text =
    if page == settingsPageName then
        decodeAtom text
    else
        let node = case parseNode text of
                       Err err ->
                           { emptyNode
                               | title = "Error parsing page: " ++ page
                               , path = page
                               , contentType = Text
                               , rawContent = text
                           }
                       Ok n ->
                           n
        in
            nodeToAtom { node | path = page }

parseNodeContent : Node msg -> Result String (Atom msg)
parseNodeContent node =
    case node.contentType of
        Json ->
            decodeAtom node.rawContent
        Markdown ->
            Markdown.run node.rawContent
                |> Utility.mergeStrings
                |> Ok
        Text ->
            Ok ( StringAtom node.rawContent )
        Code ->
            Ok ( RecordAtom
                     { tag = "pre"
                     , attributes = []
                     , body = [ StringAtom node.rawContent ]
                     }
               )

nodeToAtom : Node msg -> Result String (Atom msg)
nodeToAtom node =
    case parseNodeContent node of
        Err s as err ->
            err
        Ok atom ->
            let plist : List (String, Atom msg)
                plist = [ ("title", StringAtom node.title)
                        , ("path", StringAtom node.path)
                        , ("author", StringAtom node.author)
                        , ("time", FloatAtom node.time)
                        -- This should be formatted according to a setting
                        -- using justinmimbs/elm-date-extra
                        , ("date", StringAtom <| toString <| Date.fromTime node.time)
                        , ("content", atom)
                        ]            
            in
                Ok
                <| FuncallAtom
                    { function = "let"
                    , args = [ PListAtom [ ("node", PListAtom plist) ]
                             , LookupTemplateAtom node.nodeTemplate
                             ]
                    }

pageFetchDone : String -> Loaders Msg Extra -> Result Http.Error String -> Model -> ( Model, Cmd Msg )
pageFetchDone name loaders result model =
    case result of
        Err err ->
            ( { model
                  | error =
                      Just
                      <| "Error fetching page " ++ name ++ ": " ++ (toString err)
              }
            , Cmd.none
            )
        Ok text ->
            case receiveCustomPage (parsePage name) name text loaders of
                Err msg ->
                    ( { model
                          | error =
                            Just
                            <| ("While loading page \"" ++ name ++ "\": " ++ msg)
                      }
                    , Cmd.none
                    )
                Ok loaders2 ->
                    continueLoading loaders2 model

maxOneLineEncodeLength : Int
maxOneLineEncodeLength =
    60

encode : Atom msg -> String
encode atom =
    let res = customEncodeAtom 0 atom
    in
        if String.length res <= maxOneLineEncodeLength then
            res
        else
            encodeAtom atom

getPlayState : Model -> PlayState Msg
getPlayState model =
    case model.playState of
        Nothing ->
            updatePlayState
                (playStringUpdate "\"Hello Xossbow!\"")
                model.loaders
                emptyPlayState
        Just playState ->
            playState                     

updatePlayString : Update -> Model -> ( Model, Cmd Msg )
updatePlayString update model =
    ( { model
          | playState
              = Just
                <| updatePlayState update model.loaders <| getPlayState model
      }
    , Cmd.none
    )

-- This will become a table, looked up via settings.backend
backend : Backend Msg
backend =
    ApachePost.backend

login : String -> String -> Model -> ( Model, Cmd Msg )
login username password model =
    ( { model | error = Just "Logging in..." }
    , backend.operator
        HandleLogin (Types.Authorize <| Authorization username password)
    )

handleLogin : (Result (String, BackendOperation) BackendOperation) -> Model -> ( Model, Cmd Msg )
handleLogin result model =
    case log "handleLogin" result of
        Ok (Types.Authorize authorization) ->
            ( { model
                  | authorization = Just authorization
                  , error = Just "Success!"
              }
            , Cmd.none)
        _ ->
            ( { model | error = Just "Login failed." }
            , Cmd.none
            )

---
--- view
----

view : Model -> Html Msg
view model =
    div []
        [ case model.page of
              Nothing ->
                  text ""
              Just page ->
                  let loaders = insertFunctions
                                [ ( "playDiv"
                                  , playDivFunction
                                      UpdatePlayState <| getPlayState model
                                  )
                                ]
                                model.loaders                                      
                      template = pageTemplate
                      content = LookupPageAtom page
                  in
                      case getTemplate template loaders of
                          Nothing ->
                              dictsDiv "Template" template loaders
                          Just tmpl ->
                              case getPage page loaders of
                                  Nothing ->
                                      dictsDiv "Page" page loaders
                                  Just atom ->
                                      let loaders2 = setAtoms
                                                     [ ("content", content)
                                                     , ("page", StringAtom page)
                                                     ]
                                                     loaders
                                      in
                                          render tmpl loaders2
        , p [ style [ ( "color", "red" ) ] ]
              [ case model.error of
                    Just err ->
                    text err
                    Nothing ->
                    text " "
              ]
        ]

dictsDiv : String -> String -> Loaders Msg Extra -> Html Msg
dictsDiv thing page loaders =
    div []
        [ p [] [ text <| thing ++ " not found: " ++ page ]
        , p []
              [ a [ href "template/" ]
                    [ text "template/"]
              ]
        , p []
            [ text "dicts:"
            , br
            , text <| toString <| getDicts loaders ]
        ]
        
br : Html Msg
br =
    Html.br [] []
