package unit.issues;

private typedef Issue10089Params = {
	var callback:String->Void;
}

private class Issue10089Callable {
	var callback:String->Void;

	public function new(params:Issue10089Params) {
		callback = params.callback;
	}

	public function invoke() {
		callback("Argument String");
	}
}

class Issue10089 extends Test {
	function test() {
		var result = "";
		var callback = function(arg:String) {
			result = arg;
		}
		var callable = new Issue10089Callable({callback: callback});
		callable.invoke();
		eq(result, "Argument String");
	}
}
