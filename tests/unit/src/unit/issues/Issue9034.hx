package unit.issues;

import unit.Test;

class Issue9034 extends Test{
	#if java
	function test() {
		bar(java.nio.file.Paths.get('build.hxml', new java.NativeArray(0)));
		utest.Assert.pass();
	}

	static public inline function bar(path:java.nio.file.Path):Void { }
	#end
}