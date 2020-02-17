package unit.issues;

class Issue5556 extends unit.Test {
	function test() {
		var x = dynamicFunc(randomCall());
		#if (!flash && !js)
		eq(1, x);
		#else
		noAssert();
		#end
	}

	static function randomCall() {
		dynamicFunc = function(x:Int) return x * 2;
		return 1;
	}

	static dynamic function dynamicFunc(x:Dynamic) return x;
}