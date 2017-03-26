# XosBow - Blogging in Elm

"XosBow" is pronounced "Crossbow".

Elm is a good wood for making bows. A Crossbow is a particularly powerful bow. XosBow is a particularly powerful blogging framework, made with [Elm](http://elm-lang.org/) and based on [billstclair/elm-html-template](http://package.elm-lang.org/packages/billstclair/elm-html-template/latest).

XosBow is pure Elm, hence it will run in `elm reactor`. To do that:

    cd ...
    git clone git@github.com:billstclair/xosbow.git
    cd xosbow
    elm reactor

Then aim your browser at http://localhost:8000/src/Xosbow.elm.

To test compile a file in the `src/` directory, e.g. Xosbow.elm:

    cd .../xosbow
    bin/m Xosbow
    
To build Xosbow, and put the resulting `index.html` into the `src/` directory:

    cd .../xosbow
    bin/build
    
To do a build and sync the `xosbow.com` website with your local machine (not too useful for anybody but me):

    cd .../xosbow
    bin/update-site

Bill St. Clair
26 March 2017
