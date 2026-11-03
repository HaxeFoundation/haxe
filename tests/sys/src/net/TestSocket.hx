package net;

import sys.net.*;
import utest.Assert;

class TestSocket extends utest.Test {
	var registeredSockets:Array<Socket> = [];

	public function register<T:Socket>(socket:T):T {
		registeredSockets.push(socket);
		return socket;
	}

	public function teardown() {
		for(socket in registeredSockets) {
			if(socket == null) continue;
			socket.close();
		}
		registeredSockets = [];
	}

	#if !js // bind is not implemented on nodejs
	public function testBind() {
		var socket = register(new Socket());
		socket.bind(new Host('localhost'), 34567);
		Assert.pass();
	}

	public function testIssue12880_rebindAfterTimeWait() {
		var localhost = new Host("127.0.0.1");
		var server = new Socket();
		server.bind(localhost, 0);
		server.listen(1);
		var port = server.host().port;

		var client = new Socket();
		client.connect(localhost, port);
		var accepted = server.accept();
		// The server's end closes first, so the connection waits in TIME_WAIT on the
		// port, as it does after a server answers a request and is then stopped.
		accepted.close();
		client.close();
		server.close();

		var again = register(new Socket());
		again.bind(localhost, port);
		Assert.pass();
	}
	#end
}
