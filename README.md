N2O: Haskell Web Framework
==========================

[![Build Status](https://secure.travis-ci.org/nponeccop/n2o.hs.png?branch=master)](http://travis-ci.org/nponeccop/n2o.hs)

Features
--------

* Formatters: **BERT**, Text
* Protocols: N2O
* Endpoints: **WebSocket**, HTTP static
* PubSub: Built-in
* DOM Language: Blaze-HTML

```
$ cloc static src/Network/N2O* | tail -n+6 | sed s/--*// | column -t
Language    files  blank  comment  code
JavaScript  6      28     10       147
Haskell     2      48     6        136
SUM:        8      76     16       283
```

Mac OS X
--------

For quickstart you need `Git` for retrieving sources, glorious `Haskell` compiler, Static Web Server `webfsd`, `Casper.js` which run on top of headless v8 `Phantom.js`:

```
   $ brew install ghc
   $ brew install cabal-install
   $ brew install webfs
   $ npm install -g casperjs
   $ git clone http://github.com/nponeccop/n2o.hs && cd n2o.hs
   $ cabal update
   $ cabal install
   $ webfsd
   $ dist/build/n2o/n2o
   $ open http://localhost:8000/sample/client.html
   $ casperjs test tests
```

Arch Linux
----------

The library and the example chat server:
```
   $ pacman -Sy --needed git ghc cabal-install webfs
   $ git clone http://github.com/nponeccop/n2o.hs && cd n2o.hs
   $ cabal update
   $ cabal install
   $ webfsd
   $ nohup dist/build/n2o/n2o &
   $ open http://localhost:8000/sample/client.html
```
The PhantomJS headless browser tests:
```
   $ pacman -S --needed npm phantomjs
   $ npm install casperjs
   $ open http://localhost:8000/sample/client.html
   $ node_modules/.bin/casperjs test tests
```

Idea
----

Small and efficient protocol stack with endpoint formatters having
session storage with select for pubsub. The protocol is binary compatible with original N2O protocol: http://5ht.co/n2o.htm

Support
-------
* [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/nponeccop/n2o.hs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
* IRC Channel #n2o on FreeNode 24/7

Credits
-------

* Andy Melnikov
* Maxim Sokhatsky
