package unit.issues;

import unit.Test;

class Issue9034 extends Test{
	#if jvm
	function test() {
		bar(java.nio.file.Paths.get('build.hxml'));
		utest.Assert.pass();
	}

	static public inline function bar(path:java.nio.file.Path):Void { }
	#end
}
