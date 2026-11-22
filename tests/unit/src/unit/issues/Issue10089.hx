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

private class Issue10089OptionalCallable {
	final callback:Null<String->Void>;

	public function new(?params:Issue10089Params) {
		callback = params == null ? null : params.callback;
	}

	public function invoke() {
		callback("Argument String");
	}
}

class Issue10089 extends Test {
	static var staticCallback:Null<String->Void>;

	function testConditional() {
		var result = "";
		var params:Issue10089Params = {callback: arg -> result = arg};
		new Issue10089OptionalCallable(params).invoke();
		eq(result, "Argument String");

		checkConditional(params, true);
		eq(result, "local");
		eq(staticCallback, null);
		checkConditional(params, false);
		eq(result, "static");
	}

	function checkConditional(params:Issue10089Params, useLocal:Bool) {
		final local = useLocal ? params.callback : null;
		if (local != null) {
			local("local");
		}
		staticCallback = useLocal ? null : params.callback;
		if (staticCallback != null) {
			staticCallback("static");
		}
	}

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
