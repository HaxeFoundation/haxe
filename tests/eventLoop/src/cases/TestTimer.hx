package cases;

import haxe.Timer;

@:timeout(10000)
@:depends(cases.TestEvents)
class TestTimer extends utest.Test {
	function testCorrectInterval(async:Async) {
		async.branch(async -> {
			var i = 0;
			var interval = 0.1;
			var t = new Timer(Std.int(interval * 1000));
			var start = Timer.stamp();
			t.run = () -> {
				var dt = Timer.stamp() - start;
				//check the interval is ~100ms
				isTrue(dt >= interval, 'Passed time ($dt seconds) is too small. At least $interval seconds expected.');
				if(i++ > 5) {
					t.stop();
					async.done();
					return;
				}
				start += interval;
			}
		});
		async.branch(async -> {
			var delay = 0.05;
			var start = Timer.stamp();
			Timer.delay(() -> {
				var dt = Timer.stamp() - start;
				//check the interval is ~50ms
				isTrue(dt >= delay, 'Passed time ($dt seconds) is too small. At least $delay seconds expected.');
				async.done();
			}, Std.int(delay * 1000));
		});
	}

	function testCallbackInSameThread(async:Async) {
		var mainThread = Thread.current();

		function work(n:Int) {
			var thread = Thread.current();
			var t = new Timer(100);
			var i = 0;
			var sameThread = true;
			t.run = () -> {
				sameThread = sameThread && thread == Thread.current();
				if(i++ > 5) {
					t.stop();
					mainThread.sendMessage({n:n, type:'interval', sameThread:sameThread});
				}
			}
			Timer.delay(() -> {
				mainThread.sendMessage({n:n, type:'delay', sameThread:thread == Thread.current()});
			}, 50);
		}

		for(n in 0...10) {
			Thread.create(work.bind(n));
		}

		//expect two messages with different types per thread
		var counters = [for(i in 0...10) ['delay', 'interval']];
		for(i in 0...20) {
			var msg:{n:Int, type:String, sameThread:Bool} = Thread.readMessage(true);
			isTrue(msg.sameThread);
			isTrue(counters[msg.n].remove(msg.type));
		}
		for(types in counters) {
			equals(0, types.length);
		}

		//test in main thread
		async.branch(async -> {
			var t = new Timer(100);
			var i = 0;
			t.run = () -> {
				isTrue(mainThread == Thread.current());
				if(i++ > 5) {
					t.stop();
					async.done();
				}
			}
		});
		async.branch(async -> {
			Timer.delay(() -> {
				isTrue(mainThread == Thread.current());
				async.done();
			}, 50);
		});
	}
}