package unit.issues;

#if php
import php.Exception;
#end

class Issue4973 extends Test {
	#if php
	function test() {
		try sys.io.File.getContent("not-existant")
		catch(exc:Exception) t(Std.is(exc, Exception))
		catch(exc:Dynamic) t(Std.is(exc, Exception));
	}
	#end
}
