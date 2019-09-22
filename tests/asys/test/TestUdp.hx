package test;

import haxe.io.Bytes;
import utest.Async;

using asys.net.AddressTools;

class TestUdp extends Test {
	#if !neko
	function testEcho(async:Async) {
		sub(async, done -> {
			var server = asys.net.UdpSocket.create(Ipv4);
			server.unref();
			server.bind("127.0.0.1".toIp(), 3232);
			server.messageSignal.on(msg -> {
				beq(msg.data, TestConstants.helloBytes);
				server.close(err -> {
					eq(err, null);
					done();
				});
			});
		});

		sub(async, done -> {
			var client = asys.net.UdpSocket.create(Ipv4);
			client.send(TestConstants.helloBytes, 0, TestConstants.helloBytes.length, "127.0.0.1".toIp(), 3232, (err) -> {
				eq(err, null);
				client.close(err -> {
					eq(err, null);
					done();
				});
			});
		});
	}

	function testEcho6(async:Async) {
		sub(async, done -> {
			var server = asys.net.UdpSocket.create(Ipv6);
			server.unref();
			server.bind(AddressTools.localhost(Ipv6), 3232);
			server.messageSignal.on(msg -> {
				beq(msg.data, TestConstants.helloBytes);
				server.close(err -> {
					eq(err, null);
					done();
				});
			});
		});

		sub(async, done -> {
			var client = asys.net.UdpSocket.create(Ipv6);
			client.send(TestConstants.helloBytes, 0, TestConstants.helloBytes.length, AddressTools.localhost(Ipv6), 3232, (err) -> {
				eq(err, null);
				client.close(err -> {
					eq(err, null);
					done();
				});
			});
		});
	}
	#end
}
