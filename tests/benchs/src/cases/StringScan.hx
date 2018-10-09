package cases;

import hxbenchmark.Suite;

using StringTools;

class StringScan extends TestCase {
	function measure20003() {
		var s = "".lpad("a", 10000) + "bcd" + "".lpad("e", 10000);
		var suite = new Suite("length 20003");
		suite.add("indexOf find", s.indexOf("bcd"));
		suite.add("indexOf nofind", s.indexOf("bce"));
		suite.add("indexOf hit", s.indexOf("bcd", 10000));
		suite.add("lastIndexOf find", s.lastIndexOf("bcd"));
		suite.add("lastIndexOf nofind", s.lastIndexOf("bce"));
		suite.add("lastIndexOf hit", s.lastIndexOf("bcd", 10003));
		suite.add("split find", s.split("bcd"));
		suite.add("split nofind", s.split("bce"));
		return suite.run();
	}
}