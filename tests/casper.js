casper.test.begin("Basic connectivity", 13, function (test)
{
	test.info("Connecting")	
	casper
	.start('http://localhost:8000/sample/client.html')
	.then(function ()
	{
		test.assertHttpStatus(200)
		test.assertTitle("Haskell WebSockets example", "Connected")
		test.assertVisible('#join-section')
		test.assertNotVisible('#chat-section')
		test.assertNotVisible('#users-section')
		test.assertElementCount('p', 0, '0 messages')
		test.info("Joining")
		this.fillSelectors('#join-form', { '#user' : 'phantomjs' }, true) 				
	})
	.wait(100, function ()
	{
		test.assertNotVisible('#join-section')
		test.assertVisible('#chat-section')
		test.assertVisible('#users-section')
		test.assertElementCount('p', 1, '1 message (logon)')
		test.assertSelectorHasText('p:last-child', 'phantomjs joined')
		test.info('Sending a message')	
		this.fillSelectors("#message-form", { '#text' : 'hello' }, true)
	})
	.wait(100, function ()
	{
		test.assertElementCount('p', 2, 'Logon and message')
		test.assertSelectorHasText('p:last-child', 'hello')
	})
	.run(function ()
	{
		test.done()
	})
})
/*
    this.echo(this.getTitle());
	
});
*/
