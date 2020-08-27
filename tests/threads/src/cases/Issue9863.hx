package cases;

import utest.Assert;
import sys.thread.Thread;

#if cpp
class NativeThreads extends utest.Test {
	public function test() {
		Assert.pass();
	}
}
#else

class Issue9863 extends utest.Test {
	static function runNative(job:()->Void) {
		#if java
			var t = new java.lang.Thread(new Task(job));
			t.start();
		#elseif cs
			var t = new cs.system.threading.Thread(job);
			t.Start();
		#else
			#error "Issue9863 is not implemented for this target"
		#end
	}

	public function test() {
		var childThread:Null<Thread> = null;
		function getJob(mainThread:Thread) {
			return () -> {
				childThread = Thread.current();
				mainThread.sendMessage('childThread ready');

				var msg = Thread.readMessage(true);
				Assert.equals('from main to child', msg);

				mainThread.sendMessage('done');
			}
		}
		runNative(getJob(Thread.current()));

		var msg = Thread.readMessage(true);
		Assert.equals('childThread ready', msg);

		childThread.sendMessage('from main to child');

		var msg = Thread.readMessage(true);
		Assert.equals('done', msg);
	}
}

#end

#if java
private class Task implements java.lang.Runnable {
	final job:()->Void;

	public function new(job:()->Void) {
		this.job = job;
	}

	public function run() {
		job();
	}
}
#end
