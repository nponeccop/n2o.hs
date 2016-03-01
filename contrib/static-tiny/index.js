var s = require('./static')

var http = require('http');

function finalHandler(req, res) {
    res.setHeader('Content-Type', 'text/plain');
    res.end("Couldn't handle the request")
}

http
	.createServer(require('./static')(process.cwd(), { maxAge: 0 }, finalHandler))
	.listen(8000)

console.log("Static files are served at http://localhost:8000")


