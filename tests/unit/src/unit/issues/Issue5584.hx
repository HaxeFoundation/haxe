package unit.issues;

import cpp.Callable;
import cpp.Object;
import haxe.io.Bytes;

class Issue5584 extends Test {
	function test() {
		var callable = new Callable<Object->Object>(cffi_decompress);
		var bytes = Bytes.alloc (100);
		var data:Dynamic = callable.call (bytes);
	}

	function cffi_decompress(a:Dynamic):Dynamic {
		return null;
	}
}