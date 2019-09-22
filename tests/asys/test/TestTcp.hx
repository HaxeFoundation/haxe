package test;

import haxe.io.Bytes;
import utest.Async;

using asys.net.AddressTools;

class TestTcp extends Test {
	#if !neko
	function testEcho(async:Async) {
		sub(async, done -> {
			var server:asys.net.Server = null;
			server = asys.Net.createServer({
				listen: Tcp({
					host: "127.0.0.1",
					port: 3232
				})
			}, client -> client.dataSignal.on(chunk -> {
				beq(chunk, TestConstants.helloBytes);
				client.write(chunk);
				client.destroy();
				server.close((err) -> {
					eq(err, null);
					done();
				});
			}));
			server.unref();
			server.listeningSignal.on(() -> {
				t(server.localAddress.match(Network(AddressTools.equals(_, "127.0.0.1".toIp()) => true, 3232)));
				done();
			});
			server.errorSignal.on(err -> assert());
		}, 2);

		sub(async, done -> {
			var client:asys.net.Socket = null;
			client = asys.Net.createConnection({
				connect: Tcp({
					host: "127.0.0.1",
					port: 3232
				})
			}, (err) -> {
					eq(err, null);
					t(client.localAddress.match(Network(AddressTools.equals(_, "127.0.0.1".toIp()) => true, _)));
					t(client.remoteAddress.match(Network(AddressTools.equals(_, "127.0.0.1".toIp()) => true, 3232)));
					client.errorSignal.on(err -> assert());
					client.write(TestConstants.helloBytes);
					client.dataSignal.on(chunk -> {
						beq(chunk, TestConstants.helloBytes);
						client.destroy((err) -> {
							eq(err, null);
							done();
						});
					});
				});
		});
	}

	@:timeout(1500)
	function testSignals(async:Async) {
		sub(async, done -> {
			var client = asys.net.Socket.create();
			client.errorSignal.on(err -> assert());
			client.lookupSignal.on(address -> {
				t(address.equals("127.0.0.1".toIp(), true));
				done();
			});
			client.connectTcp({
				port: 10123,
				host: "localhost",
				family: Ipv4
			}, (err:haxe.Error) -> {
					switch (err.type) {
						case UVError(asys.uv.UVErrorType.ECONNREFUSED):
							client.destroy();
							done();
						case _:
							assert();
					}
				});
		}, 2);
	}
	#end
}
