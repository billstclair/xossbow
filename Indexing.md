# Indexing in Xossbow

One of my principles in building Xossbow is to work with the barest of key/value stores as a backend. Reading has to be open, and writing has to be protected, by at least a username/password pair. The backend supports reading a file, assuring that there is authorization to make changes, writing a file, and deleting a file, and nothing more. From `Xossbow.Types`:

    type BackendOperation
        = DownloadFile UploadType String (Maybe String)
        | Authorize Authorization
        | UploadFile Authorization UploadType String String
        | DeleteFile Authorization UploadType String

Note that there's no directory listing operation. That's intentional. I don't want to rely on it. This means that indexing must be done by writing files with known names that reference files that are dynamically created by a Xossbow site's authors. This file documents that indexing mechanism.

# Backends

I currently plan only two backends:

1. [`Xossbow.Backend.ApachePost`](src/Xossbow/Backend/ApachePost.elm)

    This backend is mostly working.
    It reads files with plain old HTTP requests, and writes them with
    POSTs to a little Perl script I wrote. I haven't yet implemented
    deletion.
    
2. [Amazon S3](https://aws.amazon.com/s3/)

   Amazon's Simple Storage Service also allows reading with plain old
   HTTP requests, so I'll be able to share that code. It has it's own
   POST mechanism for writing files, with its own authorization
   mechanism, so I'll have to implement that. Since I want Xossbow to
   remain pure Elm, runnable in `elm-reactor` (except for the
   server-side scripts), I'll have to write the update code myself.
   
It appears that Evan is working on [pluggable Elm modules](https://groups.google.com/d/msg/elm-dev/qdu3NqOqGrY/_jRl5ROiBAAJ), which can be
loaded after an application starts. If so, that will enable more
backends, which can be independently distributed. Or maybe I'm
dreaming, and interpreting his March 7 status report to mean what I
want it to mean and not what he actually wrote.

# File format

A Xossbow content file, which the code in [Xossbow.Parsers](src/Xossbow/Parsers.elm) calls a `Node` ([Drupal](https://www.drupal.org/) teminology), begins with a header, which looks like a JavaScript object with strings for values. The rest of the file is in a format defined by the header:

[`site/page/index.txt`](site/page/index.txt):
    
    { version: "1"
    , nodeTemplate: "index"
    , title: "Index"
    , author: "Bill St. Clair"
    , contentType: "Json"
    }
    
    ["@blog"]

[`site/page/about.txt`](site/page/about.txt):

    { version: "1"
    , title: "About"
    , author: "Bill St. Clair"
    }
    
    Xossbow is pronounced "Crossbow".
    Elm is a good wood for making longbows. A crossbow is a particularly
    powerful bow. Xossbow is a particularly powerful blogging framework,
    made with [Elm](http://elm-lang.org/) and
    [HtmlTemplate](http://package.elm-lang.org/packages/billstclair/elm-html-template/latest).
    
    ...


The header specifies values for most of the fields in the `Xossbow.Types.Node` type:

    type alias Node msg =
        { version : Int
        , comment : String
        , pageTemplate : String
        , nodeTemplate : String
        , title : String
        , path : String
        , author : String
        , time : Time
        , contentType : ContentType
        , rawContent : String
        , content : Atom msg
        , plist : Plist
        }

Only the fields with values different from `Xossbow.Types.emptyNode` are written (and the "version" field is always written):

    emptyNode : Node msg
    emptyNode =
        { version = nodeVersion
        , comment = ""
        , pageTemplate = "page"
        , nodeTemplate = "node"
        , title = "Untitled"
        , path = "nada"
        , author = "Unknown"
        , time = -433540800000 + (8 * 3600 * 1000)
        , contentType = Markdown
        , rawContent = "You were expecting maybe a treatise?"
        , content = ListAtom []
        , plist = []
        }

And only the fields copied by `Xossbow.Parsers.nodeToPlist` are ever saved:

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
