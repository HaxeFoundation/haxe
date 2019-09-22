package test;

import haxe.io.*;
import impl.*;
import utest.Async;

class TestStreams extends Test {
	static var bytes123 = ["1", "22", "333"].map(Bytes.ofString.bind(_, null));
	static var bytes555 = ["aaaaa", "bbbbb", "ccccc"].map(Bytes.ofString.bind(_, null));

	function testRead(async:Async) {
		var calls = [];
		var stream = new SlowSource(bytes123);
		stream.dataSignal.on((chunk) -> calls.push(chunk.length));
		stream.endSignal.once(() -> {
			aeq(calls, [1, 2, 3]);
			async.done();
		});
	}

	function testReadHWM(async:Async) {
		sub(async, done -> {
			var calls = [];
			var stream = new FastSource(bytes555, 4);
			stream.dataSignal.on((chunk) -> {
				eq(stream.bufferLength, 0);
				calls.push(chunk.length);
			});
			stream.endSignal.once(() -> {
				aeq(calls, [5, 5, 5]);
				done();
			});
		});

		sub(async, done -> {
			var lens = [];
			var calls = [];
			var stream = new FastSource(bytes555, 15);
			stream.dataSignal.on((chunk) -> {
				lens.push(stream.bufferLength);
				calls.push(chunk.length);
			});
			stream.endSignal.once(() -> {
				aeq(lens, [10, 5, 0]);
				aeq(calls, [5, 5, 5]);
				done();
			});
		});

		sub(async, done -> {
			var lens = [];
			var calls = [];
			var stream = new FastSource(bytes555, 10);
			stream.dataSignal.on((chunk) -> {
				lens.push(stream.bufferLength);
				calls.push(chunk.length);
			});
			stream.endSignal.once(() -> {
				aeq(lens, [5, 0, 0]);
				aeq(calls, [5, 5, 5]);
				done();
			});
		});
	}

	function testPassiveRead(async:Async) {
		var calls = [];
		var stream = new SlowSource(bytes123);

		// add a listener but immediately pause - no calls should be made yet
		stream.dataSignal.on((chunk) -> calls.push(10 + chunk.length));
		stream.pause();

		Sys.sleep(.05);

		stream.dataSignal.on((chunk) -> calls.push(chunk.length));
		stream.endSignal.once(() -> {
			aeq(calls, [11, 1, 12, 2, 13, 3]);
			async.done();
		});
	}
}
