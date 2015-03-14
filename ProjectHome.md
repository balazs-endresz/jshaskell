### Contents ###
  * jshaskell: a library that provides some Haskell language features implemented in javascript
  * base: a port of the Haskell [base package](http://haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/index.html) (in progress)
  * jsparsec: a [Parsec](http://hackage.haskell.org/package/parsec) port ([info, and previous versions](http://code.google.com/p/jsparsec/))
  * WebBits: a port the [WebBits](http://hackage.haskell.org/package/WebBits-2.0) library
  * pretty: a port of the Haskell "pretty" package: http://hackage.haskell.org/package/pretty-1.0.1.1
  * build.hs: parses [IntelliSense](http://msdn.microsoft.com/en-us/library/bb385682.aspx) annotations to manage dependencies and import functions from modules as local variables
  * d8.exe: the developer shell of the V8 JavaScript engine, required for the build

### Usage ###
http://code.google.com/p/jshaskell/source/browse/trunk/example/src/parsejs.js

you can run the compiled code directly in Firebug or V8 for instance:

http://code.google.com/p/jshaskell/source/browse/trunk/example-parsejs.js