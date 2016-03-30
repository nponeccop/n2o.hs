Static Web Server
=================

Here we have a simple server that serves static files and demonstrates how one could use the
http-server library. For test purpose we handle only HTTP GET requests and few MIME types.

Installation & Launch
=====================

```
$ stack build 
$ ./.stack-work/dist/x86_64/Cabal-1.22.4.0/build/StaticWebServer/StaticWebServer.exe
```

Open http://localhost:8000/client.html in your browser.
Make sure that `n2o.exe` is running and feel free to test it.
