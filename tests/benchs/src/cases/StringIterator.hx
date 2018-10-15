package cases;

import hxbenchmark.Suite;
import haxe.iterators.StringKeyValueIteratorUnicode;
using StringTools;

class StringIterator extends TestCase {
	function measureL100() {
		var s = "".lpad("a", 100);
		var suite = new Suite("length 100");
		suite.add("0...length + fastCodeAt",
			for (key in 0...s.length) {
				var value = s.fastCodeAt(key);
			}
		);
		suite.add("StringKeyValueIteratorUnicode",
			for (key => value in new StringKeyValueIteratorUnicode(s)) {

			}
		);
		return suite.run();
	}

	function measureL1000() {
		var s = "".lpad("a", 1000);
		var suite = new Suite("length 1000");
		suite.add("0...length + fastCodeAt",
			for (key in 0...s.length) {
				var value = s.fastCodeAt(key);
			}
		);
		suite.add("StringKeyValueIteratorUnicode",
			for (key => value in new StringKeyValueIteratorUnicode(s)) {

			}
		);
		return suite.run();
	}

	function measureL10000() {
		var s = "".lpad("a", 10000);
		var suite = new Suite("length 10000");
		suite.add("0...length + fastCodeAt",
			for (key in 0...s.length) {
				var value = s.fastCodeAt(key);
			}
		);
		suite.add("StringKeyValueIteratorUnicode",
			for (key => value in new StringKeyValueIteratorUnicode(s)) {

			}
		);
		return suite.run();
	}

	function measure4BytesL100() {
		var s = "".lpad("𠜎", 100);
		var suite = new Suite("length 100 of 4-bytes characters");
		suite.add("0...length + fastCodeAt",
			for (key in 0...s.length) {
				var value = s.fastCodeAt(key);
			}
		);
		suite.add("StringKeyValueIteratorUnicode",
			for (key => value in new StringKeyValueIteratorUnicode(s)) {

			}
		);
		return suite.run();
	}
}