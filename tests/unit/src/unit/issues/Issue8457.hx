package unit.issues;

class Issue8457 extends unit.Test {
#if cpp
	function test() {
		var content = 'Hello world, testing, 1, 2, 3...';
		var bytes = haxe.io.Bytes.ofString(content);
		var s = cpp.Lib.stringReference(bytes);
		eq(content, s);
	}
#end
}
