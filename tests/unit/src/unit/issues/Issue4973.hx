package unit.issues;

class Issue4973 extends Test {
	#if php
	function test() {
		try sys.io.File.getContent("not-existant")
		catch(exc:php.Exception) t(untyped __php__("{0} instanceof \\Exception", exc))
		catch(exc:Dynamic) t(untyped __php__("{0} instanceof Exception", exc));
	}
	#end
}
