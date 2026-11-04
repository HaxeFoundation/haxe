package cases;

import hxbenchmark.Suite;
import hxcoro.Coro.*;
import hxcoro.CoroRun;
import hxcoro.ds.channels.Channel;

// the results for each suite should be roughly linear, so 100%/10%/1%
class Coro extends TestCase {
	function measureYield() {
		var suite = new Suite("yield");
		suite.add("100", CoroRun.runScoped(node -> {
			for (i in 0...100) {
				yield();
			}
		}));
		suite.add("1000", CoroRun.runScoped(node -> {
			for (i in 0...1000) {
				yield();
			}
		}));
		suite.add("10000", CoroRun.runScoped(node -> {
			for (i in 0...10000) {
				yield();
			}
		}));
		return suite.run();
	}

	function measureAsyncYield() {
		var suite = new Suite("asyncYield");
		suite.add("100", CoroRun.runScoped(node -> {
			for (i in 0...100) {
				node.async(_ -> yield());
			}
		}));
		suite.add("1000", CoroRun.runScoped(node -> {
			for (i in 0...1000) {
				node.async(_ -> yield());
			}
		}));
		suite.add("10000", CoroRun.runScoped(node -> {
			for (i in 0...10000) {
				node.async(_ -> yield());
			}
		}));
		return suite.run();
	}

	function measureLazyYield() {
		var suite = new Suite("lazyYield");
		suite.add("100", CoroRun.runScoped(node -> {
			for (i in 0...100) {
				node.lazy(_ -> yield());
			}
		}));
		suite.add("1000", CoroRun.runScoped(node -> {
			for (i in 0...1000) {
				node.lazy(_ -> yield());
			}
		}));
		suite.add("10000", CoroRun.runScoped(node -> {
			for (i in 0...10000) {
				node.lazy(_ -> yield());
			}
		}));
		return suite.run();
	}

	function measureHoisting() {
		var suite = new Suite("hoising");
		suite.add("100", CoroRun.runScoped(node -> {
			var a = 0;
			for (i in 0...100) {
				a++;
				yield();
			}
		}));
		suite.add("1000", CoroRun.runScoped(node -> {
			var a = 0;
			for (i in 0...1000) {
				a++;
				yield();
			}
		}));
		suite.add("10000", CoroRun.runScoped(node -> {
			var a = 0;
			for (i in 0...10000) {
				a++;
				yield();
			}
		}));
		return suite.run();
	}

	function measureChannel() {
		var suite = new Suite("channel");
		suite.add("100", CoroRun.runScoped(node -> {
			var ch = Channel.createUnbounded({});
			for (i in 0...100) {
				node.async(_ -> {
					ch.writer.write(1);
				});
			}
			for (i in 0...100) {
				node.async(_ -> {
					ch.reader.read();
				});
			}
		}));
		suite.add("1000", CoroRun.runScoped(node -> {
			var ch = Channel.createUnbounded({});
			for (i in 0...1000) {
				node.async(_ -> {
					ch.writer.write(1);
				});
			}
			for (i in 0...1000) {
				node.async(_ -> {
					ch.reader.read();
				});
			}
		}));
		suite.add("10000", CoroRun.runScoped(node -> {
			var ch = Channel.createUnbounded({});
			for (i in 0...10000) {
				node.async(_ -> {
					ch.writer.write(1);
				});
			}
			for (i in 0...10000) {
				node.async(_ -> {
					ch.reader.read();
				});
			}
		}));
		return suite.run();
	}
}