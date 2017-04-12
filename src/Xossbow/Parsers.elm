----------------------------------------------------------------------
--
-- Parsers.elm
-- Xossbow parsers
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Xossbow.Parsers exposing ( parseNode, nodeParser
                                , parsePlist, plistParser
                                , encodeNode, nodeToPlist, encodePlist
                                , parseValue, valueParser
                                , parseKeyColonValue, keyColonValueParser
                                , escapeValue
                                , testNode
                                )

import Xossbow.Types as Types
    exposing ( Node, nodeVersion, emptyNode
             , ContentType (..), Plist
             )

import Date exposing ( Date )
import Time exposing ( Time )

import Parser exposing ( Parser, Error, Count(..)
                       , (|.), (|=)
                       , oneOf, andThen, succeed, fail, source
                       , zeroOrMore, oneOrMore, keep, ignore, repeat, keyword
                       )

---
--- Parsers
---

parseNode : String -> Result Error (Node msg)
parseNode string =
    Parser.run nodeParser string

makeNode : Plist -> String -> Node msg
makeNode plist rawContent =
    let node = { emptyNode
                   | plist = plist
                   , rawContent = rawContent
               }
    in
        setComment (plist, node)
        |> setPageTemplate
        |> setNodeTemplate
        |> setTitle
        |> setPath
        |> setAuthor
        |> setTime
        |> setContentType
        |> Tuple.second

stringToContentType : String -> ContentType
stringToContentType string =
    case String.toLower string of
        "json" -> Json
        "markdown" -> Markdown
        "code" -> Code
        _ -> Text

contentTypeToString : ContentType -> String
contentTypeToString contentType =
    case contentType of
        Json -> "Json"
        Markdown -> "Markdown"
        Code -> "Code"
        Text -> "Text"

setField : String -> (Plist, Node msg) -> ( String -> Node msg -> Node msg)-> (Plist, Node msg)
setField field (plist, node) setter =
    case Types.get field plist of
        Nothing ->
            (plist, node)
        Just value ->
            (plist, setter value node)

setComment : (Plist, Node msg) -> (Plist, Node msg)
setComment pn =
    setField "comment" pn (\value node -> { node | comment = value })

setPageTemplate : (Plist, Node msg) -> (Plist, Node msg)
setPageTemplate pn =
    setField "pageTemplate" pn (\value node -> { node | pageTemplate = value })

setNodeTemplate : (Plist, Node msg) -> (Plist, Node msg)
setNodeTemplate pn =
    setField "nodeTemplate" pn (\value node -> { node | nodeTemplate = value })

setTitle : (Plist, Node msg) -> (Plist, Node msg)
setTitle pn =
    setField "title" pn (\value node -> { node | title = value })

setPath : (Plist, Node msg) -> (Plist, Node msg)
setPath pn =
    setField "path" pn (\value node -> { node | path = value })

setAuthor : (Plist, Node msg) -> (Plist, Node msg)
setAuthor pn =
    setField "author" pn (\value node -> { node | author = value })

setTime : (Plist, Node msg) -> (Plist, Node msg)
setTime pn =
    setField "time" pn
        (\value node ->
             case String.toFloat value of
                 Err _ ->
                     node
                 Ok int ->
                     { node | time = int }
        )

setContentType : (Plist, Node msg) -> (Plist, Node msg)
setContentType pn =
    setField "contentType" pn
        (\value node ->
             { node | contentType
                   = stringToContentType value
             }
        )

nodeParser : Parser (Node msg)
nodeParser =
    succeed makeNode
        |= plistParser
        |. oneOf [ ignore (Exactly 1) ((==) '\n')
                 , succeed ()
                 ]
        |= keep zeroOrMore (\_ -> True)
        |. Parser.end

parsePlist : String -> Result Error Plist
parsePlist string =
    Parser.run plistParser string

plistParser : Parser Plist
plistParser =
    Parser.delayedCommitMap
        (\x y -> x)
        (succeed identity
        |. ignore (Exactly 1) ((==) '{')
        |= oneOf
             [ Parser.delayedCommitMap
                   (\x y -> x)
                   (succeed (::)
                   |. ignore zeroOrMore isWhitespaceChar
                   |= keyColonValueParser
                   |= Parser.repeat zeroOrMore
                        (succeed identity
                        |. ignore (Exactly 1) ((==) ',')
                        |. ignore zeroOrMore isWhitespaceChar
                        |= keyColonValueParser
                        )
                   )
                   <| succeed ()
             , succeed identity
             |. ignore zeroOrMore isWhitespaceChar
             |= succeed []
             ]
        |. ignore (Exactly 1) ((==) '}')
        )
        <| succeed ()

escapedCharParser : Parser String -> Parser String
escapedCharParser parser =
    oneOf [ Parser.delayedCommit
                (ignore (Exactly 1) ((==) '\\'))
                <| keep (Exactly 1) (\_ -> True)
          , parser
          ]

parseValue : String -> Result Error String
parseValue string =
    Parser.run valueParser string

valueParser : Parser String
valueParser =
    succeed String.concat
        |= repeat zeroOrMore
           (escapedCharParser
                <| keep (Exactly 1) <| ((/=) '"')
           )

parseKeyColonValue : String -> Result Error (String, String)
parseKeyColonValue string =
    Parser.run keyColonValueParser string

keyColonValueParser : Parser (String, String)
keyColonValueParser =
    succeed (,)
        |= (keep oneOrMore <| nonWhitespaceOrChars [':', ',', '}', '"'])
        |. ignore zeroOrMore isWhitespaceChar
        |. ignore (Exactly 1) ((==) ':')
        |. ignore zeroOrMore isWhitespaceChar
        |. ignore (Exactly 1) ((==) '"')
        |= valueParser
        |. ignore (Exactly 1) ((==) '"')
        |. ignore zeroOrMore isWhitespaceChar

isWhitespaceChar : Char -> Bool
isWhitespaceChar char =
    List.member char [ ' ', '\n' ]

notChars : List Char -> Char -> Bool
notChars chars char =
    not <| List.member char chars

nonWhitespaceOrChars : List Char -> Char -> Bool
nonWhitespaceOrChars chars char =
    not <| (isWhitespaceChar char) || (List.member char chars)

---
--- Encoders
---

nodeToPlist : Node msg -> Plist
nodeToPlist node =
    [ ( "version", toString node.version )
    , ( "comment", node.comment )
    , ( "pageTemplate", node.pageTemplate )
    , ( "nodeTemplate", node.nodeTemplate )
    , ( "title", node.title )
    , ( "path", node.path )
    , ( "author", node.author )
    , ( "time", toString node.time )
    , ( "contentType", contentTypeToString node.contentType )
    ]

escapeValue : String -> String
escapeValue string =
    let chars = List.map String.fromChar <| String.toList string
        escaped = List.map (\x -> if x == "\"" then "\\\"" else x)
                  chars
    in
        String.concat escaped

encodePlist : Plist -> String
encodePlist plist =
    "{ "
    ++ (String.join "\n, "
            <| List.map (\(k, v) -> k ++ ": \"" ++ (escapeValue v) ++ "\"")
                        plist
       )
    ++
    "\n }"

removePlistDefaults : Plist -> Plist -> Plist
removePlistDefaults defaults plist =
    List.filter (\cell -> not <| List.member cell defaults) plist

emptyPlist : Plist
emptyPlist =
    nodeToPlist { emptyNode | version = -1 }

strippedPlist : Plist -> Plist
strippedPlist plist =
    removePlistDefaults emptyPlist plist

encodeNode : Node msg -> String
encodeNode node =
    ( encodePlist <| strippedPlist (nodeToPlist node) )
    ++ "\n"
    ++ (if "\n" == (String.left 1 node.rawContent) then "" else "\n")
    ++ node.rawContent

testNode1 : Node msg
testNode1 =
    { emptyNode
        | title = "I \"Loved\" Led Zeppelin!"
        , path = "i-loved-led-zeppelin"
        , author = "Joe"
        , rawContent = "I saw Led Zeppelin in 1973. Yow! They rocked!"
    }

testNode : Node msg
testNode =
    encodeNode testNode1
        |> parseNode
        |> Result.withDefault emptyNode

