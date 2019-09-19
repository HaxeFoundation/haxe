package test;

import haxe.io.Bytes;
import utest.Async;

class TestIpc extends Test {
	function testEcho(async:Async) {
		if (Sys.systemName() == "Windows") { // TODO
			t(true);
			async.done();
			return;
		}

		sub(async, done -> {
			var server:asys.net.Server = null;
			server = asys.Net.createServer({
				listen: Ipc({
					path: "resources-rw/ipc-pipe"
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
			server.errorSignal.on(err -> assert());
		});

		sub(async, done -> {
			var client:asys.net.Socket = null;
			client = asys.Net.createConnection({
				connect: Ipc({
					path: "resources-rw/ipc-pipe"
				})
			}, (err) -> {
					eq(err, null);
					t(client.remoteAddress.match(Unix("resources-rw/ipc-pipe")));
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

		TestBase.uvRun();
	}

	/*
	// TODO: this segfaults ?
	function testIpcEcho(async:Async) {
		var proc = TestBase.helperStart("ipcEcho", [], {
			stdio: [Ipc, Inherit, Inherit]
		});
		proc.messageSignal.on((message:{message:{a:Array<Int>, b:String, d:Bool}}) -> {
			t(switch (message.message) {
				case {a: [1, 2], b: "c", d: true}: true;
				case _: false;
			});
			trace("ok, closing?");
			proc.close(err -> {
				trace("closed?", err);
				eq(err, null);
				async.done();
			});
		});
		proc.send({message: {a: [1, 2], b: "c", d: true}});

		TestBase.uvRun();
	}
	*/
}
