package cases;

import hxbenchmark.Suite;
using StringTools;

@:analyzer(no_local_dce)
class CharacterAccess extends TestCase {
	function measureL1() {
		var s = "a";
		var suite = new Suite("length 1");
		suite.add("charAt(0)", s.charAt(0));
		suite.add("charCodeAt(0)", s.charCodeAt(0));
		suite.add("fastCodeAt(0)", s.fastCodeAt(0));
		return suite.run();
	}

	function measureL100() {
		var s = "".lpad("a", 100);
		var suite = new Suite("length 100");
		suite.add("charAt(0)", s.charAt(0));
		suite.add("charAt(50)", s.charAt(50));
		suite.add("charAt(99)", s.charAt(99));
		suite.add("charCodeAt(0)", s.charCodeAt(0));
		suite.add("charCodeAt(50)", s.charCodeAt(50));
		suite.add("charCodeAt(99)", s.charCodeAt(99));
		suite.add("fastCodeAt(0)", s.fastCodeAt(0));
		suite.add("fastCodeAt(50)", s.fastCodeAt(50));
		suite.add("fastCodeAt(99)", s.fastCodeAt(99));
		suite.add("charAt(erratic)", {
			s.charAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		suite.add("charCodeAt(erratic)", {
			s.charCodeAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		suite.add("fastCodeAt(erratic)", {
			s.fastCodeAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		return suite.run();
	}

	function measureL10000() {
		var s = "".lpad("a", 10000);
		var suite = new Suite("length 10000");
		suite.add("charAt(0)", s.charAt(0));
		suite.add("charAt(5000)", s.charAt(5000));
		suite.add("charAt(9999)", s.charAt(9999));
		suite.add("charCodeAt(0)", s.charCodeAt(0));
		suite.add("charCodeAt(5000)", s.charCodeAt(5000));
		suite.add("charCodeAt(9999)", s.charCodeAt(9999));
		suite.add("fastCodeAt(0)", s.fastCodeAt(0));
		suite.add("fastCodeAt(5000)", s.fastCodeAt(5000));
		suite.add("fastCodeAt(9999)", s.fastCodeAt(9999));
		suite.add("charAt(erratic)", {
			s.charAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		suite.add("charCodeAt(erratic)", {
			s.charCodeAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		suite.add("fastCodeAt(erratic)", {
			s.fastCodeAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		return suite.run();
	}

	function measureL1000000() {
		var s = "".lpad("a", 1000000);
		var suite = new Suite("length 1000000");
		suite.add("charAt(0)", s.charAt(0));
		suite.add("charAt(500000)", s.charAt(500000));
		suite.add("charAt(999999)", s.charAt(999999));
		suite.add("charCodeAt(0)", s.charCodeAt(0));
		suite.add("charCodeAt(500000)", s.charCodeAt(500000));
		suite.add("charCodeAt(999999)", s.charCodeAt(999999));
		suite.add("fastCodeAt(0)", s.fastCodeAt(0));
		suite.add("fastCodeAt(500000)", s.fastCodeAt(500000));
		suite.add("fastCodeAt(999999)", s.fastCodeAt(999999));
		suite.add("charAt(erratic)", {
			s.charAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		suite.add("charCodeAt(erratic)", {
			s.charCodeAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		suite.add("fastCodeAt(erratic)", {
			s.fastCodeAt(index);
			index += index;
			index %= s.length;
		}, var index = 0);
		return suite.run();
	}
}