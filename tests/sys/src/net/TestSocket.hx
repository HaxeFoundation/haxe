package net;

import sys.net.*;
import utest.Assert;

class TestSocket {
	var registeredSockets:Array<Socket> = [];

	public function register<T:Socket>(socket:T):T {
		registeredSockets.push(socket);
		return socket;
	}

	public function tearDown() {
		for(socket in registeredSockets) {
			if(socket == null) continue;
			socket.close();
		}
		registeredSockets = [];
	}

	public function new() { }

	public function testBind() {
		var socket = register(new Socket());
		socket.bind(new Host('localhost'), 34567);
		Assert.pass();
	}
}