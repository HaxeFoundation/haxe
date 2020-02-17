package cases;

import hxbenchmark.Suite;

using StringTools;

class StringCreate extends TestCase {
	@:analyzer(no_optimize)
	function measureCreate() {
		var suite = new Suite("10000 iterations");
		var s100 = StringTools.lpad("", "abcdefghijklmnopqrstuvwxzy", 100);
		var s1000 = StringTools.lpad("", "abcdefghijklmnopqrstuvwxzy", 1000);
		var s10000 = StringTools.lpad("", "abcdefghijklmnopqrstuvwxzy", 10000);
		suite.add("concat 0", {
			var s = "";
			for (i in 0...10000) {
				s += "";
			}
		});
		suite.add("concat 1", {
			var s = "";
			for (i in 0...10000) {
				s += "a";
			}
		});
		// suite.add("concat 100", {
		// 	var s = "";
		// 	for (i in 0...10000) {
		// 		s += s100;
		// 	}
		// });
		// suite.add("concat 1000", {
		// 	var s = "";
		// 	for (i in 0...10000) {
		// 		s += s1000;
		// 	}
		// });
		suite.add("substr 100", {
			var s100 = s100;
			for (i in 0...10000) {
				s100.substr(13, 1);
			}
		});
		suite.add("substr 1000", {
			var s1000 = s1000;
			for (i in 0...10000) {
				s1000.substr(13, 1);
			}
		});
		// suite.add("substr 10000", {
		// 	var s10000 = s10000;
		// 	for (i in 0...10000) {
		// 		s10000.substr(13, 1);
		// 	}
		// });
		suite.add("replace 100", {
			var s100 = s100;
			for (i in 0...10000) {
				s100.replace("l", "L");
			}
		});
		// suite.add("replace 1000", {
		// 	var s1000 = s1000;
		// 	for (i in 0...10000) {
		// 		s1000.replace("l", "L");
		// 	}
		// });
		return suite.run();
	}
}