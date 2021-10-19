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
	#end
}
