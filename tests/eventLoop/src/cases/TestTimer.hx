package cases;

import haxe.Timer;

@:timeout(5000)
class TestTimer extends utest.Test {
	function testCorrectInterval(async:Async) {
		async.branch(async -> {
			var t = new Timer(100);
			var i = 0;
			var start = Timer.stamp();
			t.run = () -> {
				var dt = Timer.stamp() - start;
				//check the interval is ~100ms
				isTrue(Math.abs(dt - 100) <= 2);
				if(i++ > 5) {
					t.stop();
					async.done();
					return;
				}
				start += 100;
			}
		});
		async.branch(async -> {
			var start = Timer.stamp();
			Timer.delay(() -> {
				var dt = Timer.stamp() - start;
				//check the interval is ~50ms
				isTrue(Math.abs(dt - 50) <= 2);
				async.done();
			}, 50);
		});
	}

	function testCallbackInSameThread(async:Async) {
		var mainThread = Thread.current();

		function work(n:Int) {
			var thread = Thread.current();
			var t = new Timer(100);
			var i = 0;
			var success = true;
			t.run = () -> {
				success = success && thread == Thread.current();
				if(i++ > 5) {
					t.stop();
					mainThread.sendMessage({n:n, type:'interval', success:success});
				}
			}
			Timer.delay(() -> {
				var success = thread && Thread.current();
				mainThread.sendMessage({n:n, type:'delay', success:success});
			}, 50);
		}

		for(n in 0...10) {
			Thread.create(work.bind(n));
		}

		//two messages with different types per thread
		var counters = [for(i in 0...10) ['delay', 'interval']];
		for(i in 0...20) {
			var msg:{n:Int, type:String, success:Bool} = Thread.readMessage(true);
			isTrue(msg.success);
			counters[msg.n].remove(msg.type);
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