package cases;

import hxbenchmark.Suite;
import haxe.iterators.StringKeyValueIteratorUnicode;
using StringTools;


@:analyzer(no_local_dce)
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
}