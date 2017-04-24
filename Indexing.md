# Indexing in Xossbow

One of my principles in building Xossbow is to work with the barest of key/value stores as a backend. Reading has to be open, and writing has to be protected, by at least a username/password pair. The backend supports reading a file, assuring that there is authorization to make changes, writing a file, and deleting a file, and nothing more. From `Xossbow.Types`:

    type BackendOperation
        = DownloadFile UploadType String (Maybe String)
        | Authorize Authorization
        | UploadFile Authorization UploadType String String
        | DeleteFile Authorization UploadType String

Note that there's no directory listing operation. That's intentional. I don't want to rely on it. This means that indexing must be done by writing files with known names that reference files that are dynamically created by a Xossbow site's authors. This file documents that indexing mechanism.

## Backends

I currently plan only three backends:

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

3. [`Xossbow.Backend.RamDict`](src/Xossbow/Backend/RamDict.elm)

   This backend is for testing. It does reads using the `ApachePost`
   backend, and writes to an in-memory `Dict`. It forced me to realize
   that a backend might need state, so they now can.

It appears that Evan is working on [pluggable Elm modules](https://groups.google.com/d/msg/elm-dev/qdu3NqOqGrY/_jRl5ROiBAAJ), which can be
loaded after an application starts. If so, that will enable more
backends, which can be independently distributed. Or maybe I'm
dreaming, and interpreting his March 7 status report to mean what I
want it to mean and not what he actually wrote.

## File format

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

[`site/page/blog.txt`](site/page/blog.txt):

    { version: "1"
    , title: "Hello Xossbow"
    , author: "Bill St. Clair"
    }
    
    This is the first post in the [["#xossbow"]] blog.
        
    I'm undecided about a logo. I currently have three ideas. If you
    have a preference, or another idea, please send email to
    <Xossbow@gmail.com>. Here are my ideas:
    
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

## Index files

The current `index.txt` gives a clue. The plan is to be able to "tag" each post, with as many tags as you want. Each tag will have its own directory, "`tag/<tag-name>`", with an `index.txt` file whose `title` describes the tag, and whose contents is the inclusion of the most recent index file with that tag. Previous and next links give you a way to navigate to older, and then younger, index pages.

`site/tag/blog/index.txt`:

    { version: "1"
    , nodeTemplate: "index"
    , title: "Blog"
    , description: "[\"$settings.siteDescription\"]"
    , author: "Bill St. Clair"
    , contentType: "Json"
    }
    
    [ "@tag/blog/30"
    ]

`site/tag/blog/30.txt`:
    
    { version: "1"
    , nodeTemplate: "index"
    , title: "Blog"
    , description: "[\"$settings.siteDescription\"]"
    , author: "Bill St. Clair"
    , contentType: "Json"
    , permindex: "30"
    , previous: "20"
    , next: ""
    }
    
    [ "@post22"
    , "@post21" 
    ]

`site/tag/blog/20.txt`:
    
    { version: "1"
    , nodeTemplate: "index"
    , title: "Blog page 2"
    , author: "Bill St. Clair"
    , contentType: "Json"
    , permindex: "20"
    , previous: "10"
    , next: "index"
    }
    
    [ "@post20"
    , "@post19" 
    , "@post18" 
    , "@post17" 
    , "@post16" 
    , "@stories/billpoopoo" 
    , "@post14" 
    , "@post13" 
    , "@post12" 
    , "@post11" 
    ]

`site/tag/blog/10.txt`:
    
    { version: "1"
    , nodeTemplate: "index"
    , title: "Blog page 1"
    , author: "Bill St. Clair"
    , contentType: "Json"
    , permindex: "10"
    , previous: ""
    , next: "20"
    }
    
    [ "@post10"
    , "@post9" 
    , "@post8" 
    , "@post7" 
    , "@stories/inshallah" 
    , "@post5" 
    , "@post4" 
    , "@post3" 
    , "@post2" 
    , "@blog" 
    ]

`site/tag/stories/index.txt`
    { version: "1"
    , nodeTemplate: "index"
    , title: "Stories"
    , description: "Stories I've Written and Collected"
    , author: "Bill St. Clair"
    , contentType: "Json"
    }
    
    [ "@tag/stories/10"
    ]


`site/blog/stories/10.txt`:

    { version: "1"
    , nodeTemplate: "index"
    , title: "Stories"
    , description: "Stories I've Written and Collected"
    , author: "Bill St. Clair"
    , contentType: "Json"
    , permindex: "10"
    , previous: ""
    , next: ""
    }
    
    [ "@stories/billpoopoo"
    , "@stories/inshallah" 
    ]

`site/tag/index.txt`:

    { version: "1"
    , nodeTemplate: "tagsIndex"
    , title: "Tags"
    , description: "An index to articles by tag"
    , author: "Bill St. Clair"
    , contentType: "Json"
    }
    
    [ ["@tag/blog/","Blog"],
      ["@tag/stories/","Stories"]
    ]

One very interesting property of this scheme is that the same files that are used to render the index pages for the web contain all the information necessary to find all the posts for each tag. The posts also need to know their tags. There will be a default tag property in "[`settings.json`](site/settings.json)", named perhaps "`defaultTags`". And a new "`indices`" property for posts, which will always point to the permanent index file, NEVER the `"index"` file:

[`site/page/blog.txt`](site/page/blog.txt):

    { version: "1"
    , title: "Hello Xossbow"
    , author: "Bill St. Clair"
    , indices: "{blog: \"10\"}"
    }
    
    This is the first post in the [["#xossbow"]] blog.
    ...

[`site/page/stories/billpoopoo.txt`](https://etwof.com/stories/billpoopoo.html) (Do click. It's a very funny story):

    { version: "1"
    , title: "From the Mouths of Babes"
    , author: "Bill St. Clair"
    , indices: "{blog: \"20\", stories: \"10\"}"
    }
    
    This is a true story. None of the names have been changed.
    ...

Note that though the values in the property list at the top of a file are strings, for the `indices` field, that string will be JSON.

There's one more thing to do. Though this is all well and good for running a Xossbow site, since it's all rendered with JavaScript, there's nothing for Google to index. Hence, if the user so chooses, Xossbow will write HTML version of the index files that link to the text files for the content, along with actual links to the site.

`site/html/index.html`:

    <html>
      <head>
        <title>Xossbow - Blogging in Elm</title>
      </head>
      <body>
        <ul>
          <li><a href='../../tag/blog/post22.txt'>Post 22</a></li>
          <li><a href='../../tag/blog/post21.txt'>Post 21</a></li>
        </ul>
        <p>
          <a href='blog/20.html'>prev</a> index next
        </p>
      </body>
    </html>

`site/html/blog/20.html`:

    <html>
      <head>
        <title>Xossbow - Blog page 2</title>
      </head>
      <body>
        <ul>
          <li><a href='../../tag/blog/post20.txt'>Post 20</a></li>
          <li><a href='../../tag/blog/post19.txt'>Post 19</a></li>
          <li><a href='../../tag/blog/post18.txt'>Post 18</a></li>
          <li><a href='../../tag/blog/post17.txt'>Post 17</a></li>
          <li><a href='../../tag/blog/post16.txt'>Post 16</a></li>
          <li><a href='../../tag/stories/billpoopoo.txt'>From the Mouths of Babes</a></li>
          <li><a href='../../tag/blog/post14.txt'>Post 14</a></li>
          <li><a href='../../tag/blog/post13.txt'>Post 13</a></li>
          <li><a href='../../tag/blog/post12.txt'>Post 12</a></li>
          <li><a href='../../tag/blog/post11.txt'>Post 11</a></li>
        </ul>
        <p>
          <a href='10.html'>prev</a> <a href='index.html'>index</a> <a href='30.html'>next</a>
        </p>
      </body>
    </html>

Or something like that. The headers will also need permalinks, so that people who arrive there from Google will know where to go for the pretty content. Or maybe Google will start simulating JavaScript web pages to properly index them.

Make it so!
