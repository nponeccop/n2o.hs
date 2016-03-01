var s = require('./static')

function finalHandler(req, res) {
	console.log({arg : arguments})
    res.setHeader('Content-Type', 'text/plain');
    res.end("Couldn't handle the request")
}

var h =	require('./static')(process.cwd(), { maxAge: 0 }) 
require('http')
	.createServer(function (req, res) { h(req, res, finalHandler.bind(null, req, res)) })
	.listen(8000)

console.log("Static files are served at http://localhost:8000")

