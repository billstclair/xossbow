# Xossbow - Blogging in Elm

"Xossbow" is pronounced "Crossbow".

Elm is a good wood for making bows. A Crossbow is a particularly powerful bow. Xossbow is a particularly powerful blogging framework, made with [Elm](http://elm-lang.org/) and based on [billstclair/elm-html-template](http://package.elm-lang.org/packages/billstclair/elm-html-template/latest).

Xossbow is pure Elm, hence it will run in `elm reactor`. To do that:

    cd ...
    git clone git@github.com:billstclair/xosbow.git xossbow
    cd xossbow
    elm reactor

Then aim your browser at http://localhost:8000/src/Xossbow.elm.

To test compile a file in the `src/` directory, e.g. Xossbow.elm:

    cd .../xossbow
    bin/m Xossbow
    
To build Xossbow, and put the resulting `index.html` into the `src/` directory:

    cd .../xossbow
    bin/build
    
To do a build and sync the `xossbow.com` website with your local machine (not too useful for anybody but me):

    cd .../xossbow
    bin/update-site

[Indexing.md](Indexing.md) contains documentation of the post file format and indexing scheme.

Bill St. Clair
26 March 2017
