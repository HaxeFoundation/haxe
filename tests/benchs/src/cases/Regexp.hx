package cases;

import hxbenchmark.Suite;

@:analyzer(ignore)
class Regexp extends TestCase {
	function measureReplace() {
		var str = StringTools.lpad('', '"', 10 * 1024);
		var r = ~/"/g;
		var suite = new Suite('~/"/g.replace(string, "")');
		suite.add("10Kb string", r.replace(str, ""));
		return suite.run();
	}
}